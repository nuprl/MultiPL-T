from typing import Generator, TypedDict, Iterable
from torch.utils.data.dataloader import IterableDataset
import torch
from scipy.stats import bootstrap
import itertools
import numpy as np
import logging

logger = logging.getLogger(__name__)


class PackedStringsInputItem(TypedDict):
    input_ids: Iterable[int]


class PackedStringsOutputItem(TypedDict):
    input_ids: torch.Tensor
    labels: torch.Tensor
    attention_mask: torch.Tensor


def _generate_from_buffer(
    max_length: int, buffer: list, max_attn_mask: torch.Tensor, device: str
) -> Generator[PackedStringsOutputItem, None, None]:
    """
    Helper function for _generate_constant_length_sequences.

    Given a buffer of tokens, generates sequences of length max_length, updating the buffer in-place.
    If len(buffer) is evenly divisible by max_length, the buffer will be empty at the end of the
    generation. Otherwise, the buffer will contain the remaining tokens.
    """
    while len(buffer) >= max_length:
        input_ids = torch.tensor(buffer[:max_length], dtype=torch.long, device=device)
        yield {
            "input_ids": input_ids,
            "labels": input_ids,
            "attention_mask": max_attn_mask,
        }
        # Drop the first max_length tokens from buffer in-place
        del buffer[:max_length]


def _generate_constant_length_sequences(
    max_length: int,
    eos_token_id: int,
    tokenized_item_generator: Generator[PackedStringsInputItem, None, None],
    skip_last_batch: bool,
    device: str,
) -> Generator[PackedStringsOutputItem, None, None]:
    """
    Helper function for PackedStringsDataset.

    A generator of tokenized items that are packed into sequences of length exactly max_length.
    Multiple items are packed and separated by eos_token_id. Longer items are split into multiple
    sequences.
    """
    buffer = []
    max_attn_mask = torch.ones(max_length, dtype=torch.long, device=device)
    # The loop below uses _generate_from_buffer to split the buffer into sequences of length
    # exactly max_length. When _generate_from_buffer returns, it shortens the buffer by
    # max_length * N (for N >= 0).
    for item in tokenized_item_generator:
        buffer.extend(item["input_ids"])
        # We insert eos_token_id after each tokenized dpcument *only if* the document does not end
        # precisely at the end of a context window. Naively, this occurs when the length of the
        # document is a multiple of the context window size. However, we also need to account for
        # the cases where the document does not start at the beginning of a context window, which
        # is why we use the following condition.
        if len(buffer) % max_length != 0:
            buffer.append(eos_token_id)
        # When the following generator concludes, the buffer will be shorter
        yield from _generate_from_buffer(
            max_length, buffer, max_attn_mask, device=device
        )

    if skip_last_batch:
        return

    # NOTE(arjun): The last item is not of length max_length.
    if len(buffer) > 0:
        input_ids = torch.tensor(buffer, dtype=torch.long, device=device)
        pad_tokens = torch.full(
            size=(max_length - len(input_ids),),
            fill_value=eos_token_id,
            dtype=torch.long,
            device=device,
        )
        padded_input_ids = torch.cat([input_ids, pad_tokens])
        attn_mask = torch.cat(
            [
                torch.ones(len(buffer), dtype=torch.long, device=device),
                torch.zeros(max_length - len(buffer), dtype=torch.long, device=device),
            ]
        )
        labels = padded_input_ids.clone()
        labels[labels == eos_token_id] = -100
        yield {
            "input_ids": padded_input_ids,
            "attention_mask": attn_mask,
            "labels": labels,
        }


def _generate_constant_length_sequences_with_epochs(
    epochs: int,
    max_length: int,
    eos_token_id: int,
    tokenized_item_generator: Generator[PackedStringsInputItem, None, None],
    skip_last_batch: bool,
    device: str,
) -> Generator[PackedStringsOutputItem, None, None]:
    for _ in range(epochs):
        yield from _generate_constant_length_sequences(
            max_length,
            eos_token_id,
            tokenized_item_generator,
            skip_last_batch,
            device=device,
        )


class PackedStringsDataset(IterableDataset):
    """
    Given a generator of tokenized documents of irregular length, packs them into training items
    of exactly max_length. The resulting items are of the form:

    {
        "input_ids": torch.Tensor of shape (max_length,)
        "labels": torch.Tensor of shape (max_length,)
        "attention_mask": torch.Tensor of shape (max_length,)
    }

    This dataset can then be used with a DataLoader to batch items.
    """

    def __init__(
        self,
        tokenized_item_generator: Generator[PackedStringsInputItem, None, None],
        max_length: int,
        eos_token_id: int,
        epochs: int,
        skip_last_batch: bool = False,
        device: str = "cpu",
    ):
        self.tokenized_item_generator = tokenized_item_generator
        self.max_length = max_length
        self.eos_token_id = eos_token_id
        self.epochs = epochs
        self.length = None
        self._skip_last_batch = skip_last_batch
        self._device = device

    def set_length_with_exact_count(self):
        """
        Exactly counts the number of items in a single epoch. At the boundary between epochs,
        there may be an item that packs documents from both epochs. So, this count may not be
        exactly correct.
        """
        num_items = sum(
            1
            for _ in _generate_constant_length_sequences(
                self.max_length,
                self.eos_token_id,
                self.tokenized_item_generator,
                self._skip_last_batch,
                "cpu",
            )
        )
        self.length = num_items * self.epochs
        return self.length

    def set_length_with_approximate_count(self, samples=1000):
        """
        Approximately counts the number of items in a single epoch by sampling `samples` items.
        """
        # A sample of the dataset
        sampled_data = itertools.islice(self.tokenized_item_generator, samples)
        # The number of tokens in each item
        sampled_token_counts = [len(item["input_ids"]) for item in sampled_data]
        bootstrap_result = bootstrap(
            data=[sampled_token_counts], statistic=np.mean, confidence_level=0.95
        )
        # The mean number of tokens in a single item
        estimated_tokens_per_item = np.mean(bootstrap_result.confidence_interval)
        estimated_tokens = (
            estimated_tokens_per_item * self.epochs * len(self.tokenized_item_generator)
        )
        estimated_training_items = int(estimated_tokens // self.max_length)
        self.length = estimated_training_items
        return self.length

    def __iter__(self) -> Generator[PackedStringsOutputItem, None, None]:
        return _generate_constant_length_sequences_with_epochs(
            self.epochs,
            self.max_length,
            self.eos_token_id,
            self.tokenized_item_generator,
            self._skip_last_batch,
            self._device,
        )

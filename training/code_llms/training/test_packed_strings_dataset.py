from .packed_strings_dataset import (
    _generate_from_buffer,
    _generate_constant_length_sequences,
    PackedStringsDataset,
)
from torch.utils.data.dataloader import DataLoader
import torch


def test_generate_even():
    buffer = [5, 10, 15, 20]
    max_attn_mask = torch.ones(2, dtype=torch.long)
    items = list(_generate_from_buffer(2, buffer, max_attn_mask))

    assert torch.equal(items[0]["input_ids"], torch.LongTensor([5, 10]))
    assert torch.equal(items[1]["input_ids"], torch.LongTensor([15, 20]))
    assert len(buffer) == 0


def test_generate_odd():
    buffer = [5, 10, 15, 20, 25]
    max_attn_mask = torch.ones(2, dtype=torch.long)
    items = list(_generate_from_buffer(2, buffer, max_attn_mask))
    assert torch.equal(items[0]["input_ids"], torch.LongTensor([5, 10]))
    assert torch.equal(items[1]["input_ids"], torch.LongTensor([15, 20]))
    assert buffer == [25]


def test_generate_constant_length_sequences_1():
    buffer = [{"input_ids": [5, 10]}, {"input_ids": [15, 20, 25]}, {"input_ids": [30]}]
    items = list(_generate_constant_length_sequences(3, -1, buffer))
    assert torch.equal(items[0]["input_ids"], torch.LongTensor([5, 10, -1]))
    assert torch.equal(items[1]["input_ids"], torch.LongTensor([15, 20, 25]))
    assert torch.equal(items[2]["input_ids"], torch.LongTensor([30, -1, -1]))


def test_generate_constant_length_sequences_2():
    buffer = [
        {"input_ids": [5, 10]},
        {"input_ids": [15, 20, 25]},
        {"input_ids": [30]},
        {"input_ids": [35]},
    ]
    items = list(_generate_constant_length_sequences(3, -1, buffer))
    assert torch.equal(items[0]["input_ids"], torch.LongTensor([5, 10, -1]))
    assert torch.equal(items[1]["input_ids"], torch.LongTensor([15, 20, 25]))
    assert torch.equal(items[2]["input_ids"], torch.LongTensor([30, -1, 35]))


def ignoretest_generate_with_data_loader():
    items = PackedStringsDataset(
        tokenized_item_generator=[{"input_ids": list(range(10))} for _ in range(10)],
        max_length=10,
        eos_token_id=-1,
    )

    for batch in DataLoader(items, batch_size=2):
        print(batch)
    assert False

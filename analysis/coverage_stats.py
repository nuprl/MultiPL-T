import datasets
import matplotlib.pyplot as plt
import numpy as np


ds = datasets.load_dataset(
    "nuprl/stack-dedup-python-testgen-starcoder-filter-v2", split="train").to_pandas()

plt.rcParams.update({'font.size': 20})

# Coverage stats
sorted_coverage = ds["coverage"].sort_values()
percentiles = np.arange(1, len(sorted_coverage) + 1) / len(sorted_coverage)
plt.figure(figsize=(10, 7))
plt.plot(sorted_coverage, percentiles, marker='.', linestyle='-', rasterized=True)
plt.title('CDF of Python Source Coverage')
plt.xlabel('Python Source Coverage (%)')
plt.ylabel('Percentile in Dataset')
plt.xticks(np.arange(0, 110, 10))
plt.yticks(np.arange(0, 1.1, 0.1))
plt.grid(True, which="both", ls="--", c='0.7')
plt.savefig('./figures/coverage_cdf.png')
plt.savefig('./figures/coverage_cdf.pdf')
# print stats
print("Coverage stats:")
print(ds["coverage"].describe())

# Test length stats
test_lengths = ds["tests"].apply(len)
sorted_test_lengths = test_lengths.sort_values()
percentiles = np.arange(1, len(sorted_test_lengths) +
                        1) / len(sorted_test_lengths)
plt.figure(figsize=(10, 7))
plt.plot(sorted_test_lengths, percentiles, marker='.', linestyle='-', rasterized=True)
plt.title('CDF of Number of Tests Per Example')
plt.xlabel('Number of Tests')
plt.ylabel('Percentile in Dataset')
plt.yticks(np.arange(0, 1.1, 0.1))
# zoom in outliers
plt.xlim(0, test_lengths.quantile(0.99))
# increase font size
xticks = np.arange(0, test_lengths.quantile(0.99), 5)
plt.xticks(xticks)
plt.grid(True, which="both", ls="--", c='0.7')
plt.savefig('./figures/test_lengths_cdf.png')
plt.savefig('./figures/test_lengths_cdf.pdf')

# print stats
print("Test length stats:")
print(test_lengths.describe())

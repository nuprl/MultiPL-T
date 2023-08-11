library(ggplot2)
library(purrr)
library(stringr)

data_files <- c(
    "../experiments/ocaml_subset_1b/results.csv",
    "../experiments/ocaml_full_1b/results.csv",
    "../experiments/rkt_full_1b/results.csv",
    "../experiments/rkt_subset_1b/results.csv"
)

read_with_path <- function(csvpath) { 
    df <- read.csv(csvpath, header = TRUE)
    df["path"] <- csvpath
    df
}

parse_path <- function(path, elt) { 
    split <- str_split_1(path, "/")
    config <- split[length(split) - 1]
    split_config <- str_split_1(config, "_")
    language <- split_config[1]
    size <- split_config[2]
    if (elt == "lang") { 
        language
    } else if (elt == "size") { 
        size
    } else { 
        stop("Invalid element")
    }
}
parse_dataset_name <- function(ds_name) { 
    split <- str_split_1(ds_name, "_")
    step_num <- unlist(split[2])
    strtoi(step_num)
}

raw_ds <- purrr::map(data_files, read_with_path) %>%
    purrr::reduce(rbind) %>%
    dplyr::group_by(path) %>%
    dplyr::mutate(
        language = map(path, function(path) { parse_path(path, "lang") }),
        num_examples = map(path, function(path) { parse_path(path, "size") }),
        step_num = map(Dataset, parse_dataset_name),
        passk = Estimate
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(language, num_examples, passk, step_num) %>%
    dplyr::mutate(
        language = unlist(language),
        num_examples = unlist(num_examples),
        step_num = unlist(step_num)
    ) %>%
    dplyr::group_by(language, num_examples) %>%
    dplyr::arrange(step_num, .by_group = TRUE) %>%
    dplyr::mutate(epoch = dplyr::row_number()) %>%
    dplyr::ungroup()

subset_ds <- raw_ds %>%
    dplyr::filter(num_examples == "subset")
full_res_ds <- raw_ds %>%
    dplyr::filter(num_examples == "full")

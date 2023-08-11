library(ggplot2)
library(purrr)
library(stringr)

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

data_files <- c(
    "../experiments/ocaml_subset_1b/results.csv",
    "../experiments/ocaml_full_1b/results.csv",
    "../experiments/rkt_full_1b/results.csv",
    "../experiments/rkt_subset_1b/results.csv",
    "../experiments/lua_full_1b/results.csv",
    "../experiments/lua_subset_1b/results.csv"
)


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

base_rkt <- data.frame(
    language = "rkt",
    passk = 0.024, # TODO(john): this number is approximate from my memory
    epoch = 0
)
base_ocaml <- data.frame(
    language = "ocaml",
    passk = 0.015, # TODO(john): this number is approximate from my memory
    epoch = 0
)
base_lua <- data.frame(
    language = "lua",
    passk = 0.10, # TODO(john): this number is approximate from my memory
    epoch = 0
)

subset_ds <- raw_ds %>%
    dplyr::filter(num_examples == "subset") %>%
    dplyr::select(language, passk, epoch) %>%
    rbind(base_rkt, base_ocaml, base_lua, .)

full_res_ds <- raw_ds %>%
    dplyr::filter(num_examples == "full") %>%
    dplyr::select(language, passk, epoch) %>%
    rbind(base_rkt, base_ocaml, base_lua, .)

subset_plot <- ggplot(subset_ds, aes(x = epoch, y = passk, color = language)) +
    geom_line() +
    geom_point() +
    scale_x_continuous("Epoch", breaks = subset_ds$epoch) +
    theme(legend.position = "bottom")

ggsave("subset.png", plot = subset_plot, device = "png", width = 10)
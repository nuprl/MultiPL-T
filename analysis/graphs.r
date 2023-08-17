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

remap_languae <- function(lang) { 
    if (lang == "ocaml") { 
        "OCaml"
    } else if (lang == "rkt") { 
        "Racket"
    } else if (lang == "lua") { 
        "Lua"
    } else { 
        stop("Invalid language")
    }
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
        language = map(path, function(path) { remap_languae(parse_path(path, "lang")) }),
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
    dplyr::mutate(num_tokens = 2048 * step_num) %>%
    dplyr::group_by(language, num_examples) %>%
    dplyr::arrange(step_num, .by_group = TRUE) %>%
    dplyr::mutate(epoch = dplyr::row_number()) %>%
    dplyr::ungroup()

base_rkt <- data.frame(
    language = "Racket",
    passk = 0.047, 
    epoch = 0, 
    step_num = 0,
    num_tokens = 0
)
base_ocaml <- data.frame(
    language = "OCaml",
    passk = 0.015, 
    epoch = 0,
    step_num = 0,
    num_tokens = 0
)
base_lua <- data.frame(
    language = "Lua",
    passk = 0.121,  
    epoch = 0,
    step_num = 0,
    num_tokens = 0
)


subset_ds <- raw_ds %>%
    dplyr::filter(num_examples == "subset") %>%
    dplyr::select(language, passk, epoch, step_num, num_tokens) %>%
    rbind(base_rkt, base_ocaml, base_lua, .)

full_res_ds <- raw_ds %>%
    dplyr::filter(num_examples == "full") %>%
    dplyr::select(language, passk, epoch, step_num, num_tokens) %>%
    rbind(base_rkt, base_ocaml, base_lua, .)

subset_plot <- ggplot(subset_ds, aes(x = step_num, y = passk, color = language)) +
    geom_line() +
    geom_point() +    
    geom_text(
            aes(label = step_num), 
            data = dplyr::filter(subset_ds, step_num != 0),
            hjust = 0, vjust = 0, size = 3, nudge_x = 1, color="black") +
    ylim(0, 0.18) + 
    scale_y_continuous(breaks = seq(0, 0.18, 0.01)) +
    expand_limits(x=0, y=0) +
    theme(legend.position = "bottom") + 
    xlab("Number of training tokens") + 
    ylab("Pass@1") +
    labs(color = "Language") +
    theme_classic()


ggsave("subset.png", plot = subset_plot, device = "png", width = 10)
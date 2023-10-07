library(ggplot2)
library(purrr)
library(stringr)

read_with_path <- function(csvpath) { 
    df <- read.csv(csvpath, header = TRUE)
    df["path"] <- csvpath
    df
}

# Get the size as the last part of the path before .csv
parse_path <- function(path) {
    split <-  str_split_1(str_replace(path, ".csv", ""), "/")
    return(str_split_1(split[length(split)], "_")[1])
}

build_base_rkt_ds <-function(nexamples, passk) { 
    data.frame(
        nexamples = nexamples,
        ntokens = 0,
        passk = passk,
        step_num = 0,
        epoch = 0
    )
}
subsample_epochs <- function (nexamples) { 
    switch(
        as.character(nexamples[[1]]),
        "5k" = c(0, 8, 16, 24, 32, 40, 48),
        "10k" = c(0, 4, 8, 12, 16, 20, 24),
        "15k" = c(0, 3, 6, 9, 12, 15, 18),
        "20k" = c(0, 2, 4, 6, 8, 10, 12),
        "25k" = c(0, 2, 4, 5, 6, 8, 10),
        "30k" = c(0, 1, 2, 4, 5, 6, 8),
        "35k" = c(0, 1, 2, 3, 4, 5, 7),
        "40k" = c(0, 1, 2, 3, 4, 5, 6),
    )
}
#####################
base_rkt_passk <- 0.047
nexamples_levels <- c("5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k")
data_files <-  list.files( 
    path = "../experiments/rkt_1b_size_ablation",
    pattern = "*.csv",
    full.names = TRUE
)
raw_ds <- map(data_files, read_with_path) %>%
    reduce(rbind) %>%
    dplyr::mutate(
        nexamples = unlist(map(path, parse_path)),
        passk = Estimate,
        step_num = StepNum
    ) %>%
    dplyr::select(nexamples, passk, step_num) %>%
    dplyr::mutate(ntokens = 2048 * step_num) %>%
    dplyr::group_by(nexamples) %>%
    dplyr::arrange(step_num, .by_group = TRUE) %>%
    dplyr::mutate(epoch = dplyr::row_number()) %>%
    dplyr::ungroup() 

base_rkt <- rbind(
    build_base_rkt_ds("5k", base_rkt_passk),
    build_base_rkt_ds("10k", base_rkt_passk),
    build_base_rkt_ds("15k", base_rkt_passk),
    build_base_rkt_ds("20k", base_rkt_passk),
    build_base_rkt_ds("25k", base_rkt_passk),
    build_base_rkt_ds("30k", base_rkt_passk),
    build_base_rkt_ds("35k", base_rkt_passk),
    build_base_rkt_ds("40k", base_rkt_passk)
)
raw_ds$nexamples <- factor(
    raw_ds$nexamples, 
    levels=nexamples_levels
)
base_rkt$nexamples <- factor(
    base_rkt$nexamples, 
    levels=nexamples_levels
)
full_df <- rbind(base_rkt, raw_ds)
sub_df <- dplyr::group_by(full_df, nexamples) %>% 
    dplyr::filter(epoch %in% subsample_epochs(dplyr::cur_group())) %>%
    dplyr::ungroup()
#########
line_plot <- function(data) { 
    ggplot(data=data, aes(x=ntokens, y=passk, color=nexamples)) +
    geom_line() +
    scale_x_continuous() +
    scale_y_continuous(
        limits = c(0, .14),
        breaks = seq(0, .14, 0.01),
    ) +
    labs(
        x = "Number of tokens",
        y = "Pass@1",
        color = "Number of examples"
    ) +
    theme_classic()
}


ggsave("rkt_1b_size_ablation_full.png", plot = line_plot(full_df), width=10)
ggsave("rkt_1b_size_ablation_sub.png", plot = line_plot(sub_df), width=10)
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
    return(str_split_1(split[length(split)], "_")[2])
}

parse_dataset_name <- function(ds_name) { 
    split <- str_split_1(ds_name, "_")
    step_num <- unlist(split[2])
    strtoi(step_num)
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

     

data_files <-  list.files( 
    path = "../experiments/rkt_size_ablation_1b",
    pattern = "*.csv",
    full.names = TRUE
)
raw_ds <- map(data_files, read_with_path) %>%
    reduce(rbind) %>%
    dplyr::mutate(
        nexamples = unlist(map(path, parse_path)),
        passk = Estimate,
        step_num = unlist(map(Dataset, parse_dataset_name))
    ) %>%
    dplyr::select(nexamples, passk, step_num) %>%
    dplyr::mutate(ntokens = 2048 * step_num) %>%
    dplyr::group_by(nexamples) %>%
    dplyr::arrange(step_num, .by_group = TRUE) %>%
    dplyr::mutate(epoch = dplyr::row_number()) %>%
    dplyr::ungroup()

base_rkt_passk <- 0.047

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

full_ds <- rbind(raw_ds, base_rkt) 
raw_ds$nexamples <- factor(
    raw_ds$nexamples, 
    levels=c("5k", "10k", "15k", "20k", "25k", "30k", "35k", "40k")
)

# all_epoch_line_plot <- ggplot(
#         full_ds,
#         aes(x = epoch, y = passk, color = nexamples, order = class_order)
#     ) +
#     geom_line() +
#     geom_point() +
#     theme_classic() +
#     scale_y_continuous(breaks = seq(0, 0.13, 0.01)) +
#     expand_limits(x = 0, y = 0) 
    
all_epoch_bar_plot <- ggplot(
        raw_ds,
        aes(x = epoch, y = passk, fill = nexamples )
    ) +
    geom_bar(
        stat = "identity", 
        position = position_dodge(preserve = "single")
    ) +
    theme_classic() +
    xlab("Epoch #") +
    ylab("Pass@1") +
    labs(fill = "# Training Examples") +
    scale_y_continuous(breaks = seq(0, 0.13, 0.01)) + 
    scale_x_continuous(breaks = seq(1, 7, 1))

#ggsave("all_epochs_line.png", plot = all_epoch_line_plot, width = 10)
ggsave("all_epochs_bar.png", plot = all_epoch_bar_plot, width = 10)
    

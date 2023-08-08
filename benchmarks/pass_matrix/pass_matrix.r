library(ggplot2)
library(purrr)
data_file <- "starcoderbase_0p2_reworded.csv"
plot_file <- "starcoderbase_0p2_reworded_pass_matrix.pdf"
plot_title <- "StarCoderBase MultiPL-E Success Ratio Matrix"

pass_matrix <- function(csvfile, plotfile, title) { 
    df <- read.csv(csvfile, header=TRUE)
    plt <- ggplot(df, aes(x = problem, y = language, fill = success_ratio)) + 
        geom_tile() +
        geom_text(
            aes(label = round(success_ratio, digits = 2)), 
            color = "white", 
            size = 2
        ) + 
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = 0.5)
        ) + 
        ggtitle(title) +
        xlab("HumanEval Problem") + 
        ylab("Programming Language") + 
        guides(fill = guide_colorbar(title = "Success Ratio"))

    ggsave(plotfile, device = "pdf", plot = plt, width = 30)
}


pass_matrix(data_file, plot_file, plot_title)

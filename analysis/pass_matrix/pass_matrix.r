library(ggplot2)
library(purrr)
library(optparse)
cli <- OptionParser(usage = "usage: %prog [options]")
add_option(cli, c("--data"), dest="data", type="character", default="starcoderbase_0p2_reworded.csv", help="Path to data file")
add_option(cli, c("--plot"), dest="plot", type="character", default="starcoderbase_0p2_reworded_pass_matrix.pdf", help="Path to plot file")
data_file <- "starcoderbase_0p2_reworded.csv"
plot_file <- "starcoderbase_0p2_reworded_pass_matrix.pdf"

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
        xlab("HumanEval Problem") + 
        ylab("Programming Language") + 
        guides(fill = guide_colorbar(title = "Success Ratio"))

    ggsave(plotfile, device = "pdf", plot = plt, width = 30)
}


pass_matrix(data_file, plot_file, plot_title)
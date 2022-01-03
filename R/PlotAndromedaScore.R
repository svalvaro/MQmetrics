#' Andromeda score for the best associated MS/MS spectrum.
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#' @param plots_per_page Establish the maximum number of plots per page.
#' @param show_median If true it will show the median of each group, as a red
#'  dashed line.By default is TRUE.
#' @param size_median The width of the median line in the plots.
#'
#' @return Plots the MaxQuant Andromeda Score.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotAndromedaScore(MQCombined)
PlotAndromedaScore <- function(MQCombined,
                            show_median = TRUE,
                            size_median = 1.5,
                            palette = "Set2",
                            plots_per_page = 5) {
    peptides <- MQCombined$peptides.txt

    variable <- Score <- NULL

    df <- peptides %>%
        select(contains(c("id", "Score", "Experiment"),
                        ignore.case = FALSE
        )) %>%
        select(-contains(c("acid", "peptide", "IDs")))


    df_out <- melt(df, id.vars = c("id", "Score"))

    df_out$variable <- gsub("Experiment", "", df_out$variable)

    # Remove missing values

    df_out <- df_out[!is.na(df_out$value), ]


    # Repeat rows n numbers of times, being n the frequency (value)
    df_expanded <- df_out[rep(rownames(df_out), df_out$value), ]


    colourCount <- length(df) - 2

    getPalette <- colorRampPalette(brewer.pal(8, palette))

    n_pages_needed <- ceiling(
        (colourCount) / plots_per_page
    )

    myplots <- list()

    for (ii in seq_len(n_pages_needed)) {
        if (colourCount < plots_per_page) {
            nrow <- colourCount
        } else {
            nrow <- plots_per_page
        }

        p <- df_expanded %>%
            group_by(variable) %>%
            ggplot(aes(x = Score, fill = variable)) +
            geom_histogram(color = "black", binwidth = 5) +
            facet_wrap_paginate(. ~ variable,
                                ncol = 1,
                                nrow = nrow,
                                page = ii) +
            theme_bw() +
            ylab("Peptide Frequency") +
            ggtitle(label = "Andromeda score") +
            scale_fill_manual(values = getPalette(colourCount)) +
            theme(legend.position = "none")

        if (show_median == TRUE) {
            median_groups <- df_expanded %>%
                group_by(variable) %>%
                summarise(median(Score))

            p <- p + geom_vline(
                data = median_groups,
                aes(xintercept = `median(Score)`, group = variable),
                color = "red",
                linetype = "dashed",
                size = size_median
            )
        }

        myplots[[ii]] <- p
    }

    return(myplots)
}

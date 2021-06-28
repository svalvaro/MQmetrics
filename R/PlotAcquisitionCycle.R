#' Acquisition Cycle and MS/MS
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#' @param plots_per_page Establish the maximum number of plots per page.
#'
#' @return Two plots per sample, one with the cycle tyme vs retention time,
#'  and MS/MS count vs retention time.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotAcquisitionCycle(MQCombined)
PlotAcquisitionCycle <- function(
    MQCombined,
    palette = "Set2",
    plots_per_page = 5) {
    msScans <- MQCombined$msScans.txt

    `Retention time` <- `Cycle time` <- `Experiment` <- `MS/MS count` <- NULL


    data_table <- msScans %>% select(contains(c(
        "Experiment", "Retention time",
        "Cycle time", "MS/MS count"
    )))

    n_samples <- length(unique(data_table$Experiment))

    n_pages_needed <- ceiling(
        n_samples / plots_per_page
    )

    colourCount <- n_samples

    getPalette <- colorRampPalette(brewer.pal(8, palette))

    myplots <- list()

    for (ii in seq_len(n_pages_needed)) {
        if (n_samples < plots_per_page) {
            nrow <- n_samples
        } else {
            nrow <- plots_per_page
        }

        a <- ggplot(data_table, aes(
            x = `Retention time`,
            y = `Cycle time`,
            colour = Experiment
        )) +
            geom_point(alpha = 0.5, show.legend = FALSE) +
            facet_wrap_paginate(. ~ Experiment,
                                ncol = 1, nrow = nrow,
                                page = ii
            ) +
            ggtitle("Cycle time") +
            theme_bw() +
            scale_color_manual(values = getPalette(colourCount))

        b <- ggplot(data_table, aes(
            x = `Retention time`,
            y = `MS/MS count`,
            colour = Experiment
        )) +
            geom_point(alpha = 0.5, show.legend = FALSE) +
            facet_wrap_paginate(. ~ Experiment,
                                ncol = 1, nrow = nrow,
                                page = ii
            ) +
            ggtitle("MS/MS count") +
            theme_bw() +
            scale_color_manual(values = getPalette(colourCount))

        # Plot them together
        c <- plot_grid(a, b)
        # Make a title
        title <- ggdraw() + draw_label("Acquisition Cycle Parameters")



        myplots[[ii]] <- plot_grid(title, c, ncol = 1, rel_heights = c(0.1, 1))
    }

    return(myplots)
}

#' Total Ion Current
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param show_max_value If TRUE, it will show the max TIC value of each sample.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#' @param plots_per_page Establish the maximum number of plots per page.
#'
#' @return Returns a plot the Total Ion Current in each sample.
#' The maximum value is also plotted.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotTotalIonCurrent(MQCombined)
PlotTotalIonCurrent <- function(MQCombined,
                                show_max_value = TRUE,
                                palette = "Set2",
                                plots_per_page = 5) {
    msmsScans <- MQCombined$msmsScans.txt

    `Retention time` <- `Total ion current` <- Experiment <- . <- NULL

    df <- msmsScans %>% select(contains(c(
        "Experiment",
        "Retention time",
        "Total ion current"
    )))

    n_samples <- length(unique(df$Experiment))

    n_pages_needed <- ceiling(
        n_samples / plots_per_page
    )

    colourCount <- n_samples

    getPalette <- colorRampPalette(brewer.pal(8, palette))


    # Implement savitzkyGolay future release


    # df_wider <- pivot_wider(df, names_from = Experiment,
    # values_from = `Total ion current`)
    #
    #
    #
    #
    # df_wider_sg <-  df_wider
    #
    # df_wider_sg[,-1][df_wider_sg[,-1] == 'NULL'] = 0
    #
    # df_wider_sg[-1] <- lapply(df_wider_sg[-1], savitzkyGolay, p = 3, w = 11,
    # m = 0)

    # df <- df[order(df$`Total ion current`),]
    #
    #
    # vector_sg <- df$`Total ion current`
    #
    # vector_sg2 <- prospectr::savitzkyGolay(
    #   X = vector_sg,
    #   m =1,
    #   p = 2,
    #   w = 3
    # )
    #
    # df$`Total ion current` <- vector_sg2
    #
    # length(vector_sg2)
    #
    # plot(x = 1:length(vector_sg),
    #      vector_sg, type = 'l')
    #
    # plot(x = 1:length(vector_sg2),
    #      vector_sg2, type = 'l')

    for (ii in seq_len(n_pages_needed)) {
        if (n_samples < plots_per_page) {
            nrow <- n_samples
        } else {
            nrow <- plots_per_page
        }

        p <- df %>% ggplot(aes(`Retention time`, `Total ion current`)) +
            geom_line(aes(colour = Experiment)) +
            facet_wrap_paginate(. ~ Experiment,
                                ncol = 1,
                                nrow = nrow,
                                page = ii,
                                scales = "fixed"
            ) +
            ggtitle("Total Ion Current") +
            theme_bw() +
            theme(legend.position = "none") +
            scale_colour_manual(values = getPalette(colourCount))

        if (show_max_value == TRUE) {
            print(p + geom_label(
                data = . %>% group_by(Experiment) %>%
                    filter(`Total ion current` == max(`Total ion current`)),
                aes(label = format(`Total ion current`,
                                    digits = 2, scientific = TRUE)), hjust = 0.5
            ))
        } else {
            print(p)
        }
    }
}

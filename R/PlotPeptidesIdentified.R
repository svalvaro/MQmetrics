#' Total number of peaks detected and sequenced
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param palette The palette from the Package RColorBrewer. By default
#' is 'Set2'.
#'
#' @return Plots the total number of unique peptide amino acid sequences
#' identified from the recorded tandem mass spectra.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotPeptidesIdentified(MQCombined)
PlotPeptidesIdentified <- function(MQCombined,
                                long_names = FALSE,
                                sep_names = NULL,
                                palette = "Set2") {
    summary <- MQCombined$summary.txt

    `Peptide Sequences Identified` <- Experiment <- NULL


    colourCount <- nrow(summary)

    getPalette <- colorRampPalette(brewer.pal(8, palette))

    b <- ggplot(summary, aes(
        x = Experiment, y = `Peptide Sequences Identified`,
        fill = Experiment
    )) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(values = getPalette(colourCount)) +
        theme_bw(base_size = 12) +
        ggtitle("Peptides Sequences Identified") +
        theme(legend.position = "none")

    if (long_names == TRUE) {
        b + scale_x_discrete(labels = function(x) {
            stringr::str_wrap(
                gsub(
                    sep_names,
                    " ",
                    x
                ),
                3
            )
        })
    } else {
        b
    }
}

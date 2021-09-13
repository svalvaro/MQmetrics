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

    `Peptide Sequences Identified` <- Experiment <- NULL
    `Peptide sequences identified` <- NULL

    # Add exception if MBR is false

    parameters <- MQCombined$parameters.txt

    MBR <- parameters$Value[
        parameters$Parameter == 'Match between runs']

    if (MBR == 'False') {

        peptides <- MQCombined$peptides.txt %>%
            select(contains('Intensity '))

        colnames(peptides) <- gsub("Intensity ", "",
                                        colnames(peptides))

        df <- data.frame(Experiment = colnames(peptides),
                         Identified = colSums(peptides != 0),
                         `Missing values` = nrow(peptides) -
                             colSums(peptides != 0)
                         )
        rownames(df) <- NULL

    } else{

        peptides <- MQCombined$peptides.txt %>%
            select(contains("Identification type"))

        colnames(peptides) <- gsub("Identification type", "",
                                   colnames(peptides))

        By_MS_MS <- str_count(peptides, "By MS/MS")
        By_matching <- str_count(peptides, "By matching")
        NAs <- str_count(peptides, 'NA')

        df <- data.frame(Experiment = colnames(peptides),
                         `By MS/MS` = By_MS_MS,
                         `By matching` = By_matching,
                         NAs = NAs)
    }

    df <- melt(df, id.vars = 'Experiment')


    a <-  ggplot(df, aes(x = Experiment,
                        y = value,
                        fill = reorder(variable, dplyr::desc(variable)))) +
        ggtitle("Peptide Identification") +
        ylab("Peptide Frequency") +
        xlab("Experiment") +
        geom_bar(stat = "identity",
                position = "stack",
                size = 0.5,
                col = "black") +
        theme(axis.title.y = element_text(margin = margin(r = 20))) +
        theme_bw() +
        scale_fill_brewer(palette = palette) +
        theme(legend.position = "bottom",
              legend.title = element_blank())

    if (long_names == TRUE) {
        a <- a + scale_x_discrete(labels = function(x) {
            stringr::str_wrap(gsub(sep_names," ", x),3)})
    }

    return(a)
}

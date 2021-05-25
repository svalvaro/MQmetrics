#' Protein Overlap Between Samples
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#'
#' @return A plot showing the protein coverage in all samples.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotProteinOverlap(MQCombined)
PlotProteinOverlap <- function(MQCombined) {
    proteinGroups <- MQCombined$proteinGroups.txt

    samples <- value <- NULL

    df <- proteinGroups %>%
        select(contains(c("Protein IDs", "peptides "))) %>%
        select(-contains(c("unique", "Majority")))

    # Make a binary long data.frame (1 = valid value, 0 = missing value)
    # It shows the present of the protein or not.

    df_bin <- df

    df_bin[, -1][df_bin[, -1] > 1] <- 1


    # Calculate the number of times that each protein has appear in each
    # experiment

    df_bin$samples <- rowSums(df_bin[, -1])

    df_bin_stat <- df_bin %>%
        group_by(samples) %>%
        summarise(value = n())


    getPalette <- colorRampPalette(brewer.pal(9, 'Blues'))

    ggplot(df_bin_stat, aes(x = "all", y = value,fill = factor(samples))) +
        geom_col(col = "white", width = 0.3) +
        scale_fill_manual(values = getPalette(nrow(df_bin_stat))) +
        ylab("Number of Proteins") +
        theme_bw() +
        ggtitle("Protein Overlap") +
        #ggtitle("Protein Overlap Between samples") +
        labs(fill = "samples",x = NULL)
}

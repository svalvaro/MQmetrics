#' Protein Overlap Between Samples
#'
#' @param proteinGroups proteinGroups.txt table from MaxQuant output.
#'
#' @return A plot showing the protein coverage in all samples.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotProteinOverlap(proteinGroups)
#'
PlotProteinOverlap <- function(MQCombined){

    proteinGroups <- MQCombined$proteinGroups.txt

    samples <- value <- NULL

    df <- proteinGroups %>% select(contains(c('Protein IDs', 'peptides '))) %>%
        select(-contains(c('unique', 'Majority')))

    # Make a binary long data.frame (1 = valid value, 0 = missing value)
    # It shows the present of the protein or not.

    df_bin <- df

    df_bin[,-1][df_bin[,-1]>1] <- 1


    # Calculate the number of times that each protein has appear in each
    # experiment

    df_bin$samples <- rowSums(df_bin[,-1])

    df_bin_stat <- df_bin %>%
        group_by(samples) %>%
        summarise(value = n())

    ggplot(df_bin_stat, aes(x = 'all', y = value, fill = as.character(samples)))+
        geom_col(col = 'white', width =  0.3)+
        scale_fill_brewer(palette = 1)+
        ylab('Number of Proteins')+
        theme_bw()+
        ggtitle('Protein Overlap Between samples')+
        labs(fill = 'samples',
             x = NULL)

}

#' Title
#'
#' @param proteinGroups
#'
#' @return
#' @export
#'
#' @examples
PlotProteinCoverageAll <- function(proteinGroups){
  df <- proteinGroups %>% select(contains(c('Protein IDs', 'peptides '))) %>%
                          select(-contains(c('unique', 'Majority')))


  # Make a binary long data.frame (1 = valid value, 0 = missing value)
  # It shows the present of the protein or not.

  df_bin <- df

  df_bin[,-1][df_bin[,-1]>1] <- 1


  # Calculate the number of times that each protein has appear in each experiment

  df_bin$samples <- rowSums(df_bin[,-1])



  df_bin_stat <- df_bin %>%
                    group_by(samples) %>%
                    summarise(value = n())

  ggplot(df_bin_stat, aes(x = 'all', y = value, fill = as.character(samples)))+
    geom_col(col = 'white')+
    scale_fill_grey(start = 0.8, end = 0.2)+
    theme_bw()+
    labs(fill = 'samples',
         x = NULL)






}

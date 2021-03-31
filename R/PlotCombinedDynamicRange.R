#' Dynamic range of all the samples combined
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param show_shade Creates a shade showing where the \code{percent_proteins} are.
#'  Default is TRUE.
#' @param percent_proteins Determines the percentage for the show_shade parameter.
#'  Default is 0.90 (90% of the proteins).
#'
#' @return Returns the dynamic range for all samples combined. Besides, it can shows where 90\% of the proteins are and their orders of abundance.
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[["proteinGroups.txt"]]
#' PlotCombinedDynamicRange(proteinGroups)
#'
#'
PlotCombinedDynamicRange <- function(proteinGroups,
                                     show_shade = TRUE,
                                     percent_proteins = 0.90 ){

  Intensity <- NULL

  rank <-  proteinGroups %>%  select(c(Intensity))
  rank <- rank[order(rank$Intensity, decreasing=TRUE), ]
  rank$Intensity <- log10(rank$Intensity)
  rank <- rank[- grep("-Inf", rank$Intensity),]

  vector1 <- seq(1:nrow(rank))
  #Plot error bar to include the 90% of the proteins, 5% on left side, 5% on the right one







  a <- ggplot(rank, aes(x=vector1,y = Intensity))+
          geom_point(colour='darkgrey', alpha=0.75, shape=21)+
          theme_bw()+
          ggtitle('Dynamic range of protein abundance all samples')+
          ylab(expression('log'[10]*'(Intensity)'))+
          xlab('Protein Abundance Rank')

  if (show_shade == TRUE){

    limits <-  (1 - percent_proteins)/2

    limits_row <- round(nrow(rank)*limits)

    upper_y <- rank$Intensity[limits_row]
    bottom_y <- rank$Intensity[nrow(rank)-limits_row]

    orders_abundance <- paste(round(upper_y-bottom_y,digits = 1), 'orders of abundance')


    a + annotate('rect',
                 xmin = limits_row,
                 xmax = nrow(rank)-limits_row,
                 ymin = bottom_y ,
                 ymax = upper_y,
                 alpha=0.3)+
        annotate('text',
                 x = nrow(rank)/2,
                 y = bottom_y,
                 label = orders_abundance)+
        annotate('text',
                 x = nrow(rank)/2,
                 y = upper_y,
                 label = paste0(percent_proteins*100, ' % of proteins represented.'))

  } else{
    a
  }

}




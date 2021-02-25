#' Plots the dynamic range of all the samples combined
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#'
#'
#' @return Returns the dynamic range for all samples combined. Besides, it can shows where 90\% of the proteins are and their orders of abundance.
#' @export
#'
#' @examples
PlotCombinedDynamicRange <- function(proteinGroups){

  Intensity <- NULL

  rank <-  proteinGroups %>%  select(c(Intensity))
  rank <- rank[order(rank$Intensity, decreasing=TRUE), ]
  rank$Intensity <- log10(rank$Intensity)
  rank <- rank[- grep("-Inf", rank$Intensity),]

  vector1 <- seq(1:nrow(rank))
  #Plot error bar to include the 90% of the proteins, 5% on left side, 5% on the right one
  hor_error <- round(nrow(rank)*0.05)

  yminimium = rank$Intensity[hor_error]

  ymaximum = rank$Intensity[nrow(rank)-hor_error]

  orders_abundance <- paste(round(yminimium-ymaximum,digits = 1), 'orders\n of abundance')


  ggplot(rank, aes(x=vector1,y = Intensity))+

    geom_point(colour='darkgrey', alpha=0.75, shape=21)+

    theme_bw()+

    ggtitle('Dynamic range of protein abundance all samples')+

    ylab(expression('log'[10]*'(Intensity)'))+

    xlab('Protein Abundance Rank')#+
#
#     annotate("errorbar", x = nrow(rank)/2, y =(ymaximum+yminimium)/2, xmin = hor_error, xmax =
#                nrow(rank)-hor_error,  colour = "black", size = 1.0)+
#
#     annotate(geom="text", x=hor_error +0.1*nrow(rank), y=(ymaximum+yminimium)/2-0.05*yminimium, label="90% \n proteins", size=5)+
#
#     annotate("errorbar", x = nrow(rank)/2, y = (ymaximum+yminimium)/2 , ymin = yminimium,
#              ymax = ymaximum ,  colour = "black", size = 1.0, width=0.1*nrow(rank))+
#
#     annotate(geom="text", x=nrow(rank)/2 +0.07*nrow(rank), y=yminimium-0.05*yminimium, label=orders_abundance, size=5)




}




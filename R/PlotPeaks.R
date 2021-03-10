#
#' Total number of peaks detected and sequenced
#'
#' @param summary The summary.txt table from  MaxQuant Output.
#' @param position_dodge_width Width overlapping columns.
#' @param font_size Size of the font in the labels.
#'
#' @return Plots the total number of peaks detected in the full scans and the total number of peaks sequenced by tandem MS.
#' @export
#'
#' @examples
PlotPeaks <- function(summary, position_dodge_width = 1, font_size = 12, long_names = FALSE, sep_names = '-', palette = 'Set2'){

   Experiment <- `Peaks Sequenced` <- Peaks <- value <- variable <- NULL

  a <- summary %>% select(c(Experiment, Peaks, `Peaks Sequenced`))
  a_melt <- melt(a, id.vars = "Experiment" )


b <- ggplot(a_melt, aes(x=Experiment, y = value, group = variable, fill= variable))+
      geom_bar(stat = 'identity', colour='black',position = position_dodge(width = position_dodge_width))+
      theme_bw(base_size = font_size)+
      ggtitle('Peaks detected and sequenced in the full scans')+
      scale_fill_brewer(palette = palette)


 if(long_names==TRUE){
   b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
 } else{
   b
 }

}

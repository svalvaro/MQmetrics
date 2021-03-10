

#' Plot Isotope Pattern and Isotope Pattern Sequenced
#'
#' @param  summary The summary.txt table from  MaxQuant Output.
#' @param position_dodge_width Width overlapping columns.
#' @param font_size Size of the font in the labels.
#'
#' @return Returns a plot Isotope Pattern and Isotope Pattern Sequenced.
#' @export
#'
#' @examples
PlotIsotopePattern <- function(summary,position_dodge_width=1, font_size=12,  long_names = FALSE, sep_names = NULL, palette = 'Set2'){
  Experiment <- `Isotope Patterns` <- `Isotope Patterns Sequenced` <- value <- variable <- NULL

  a <- summary %>% select(c(Experiment, `Isotope Patterns`, `Isotope Patterns Sequenced`))
  a_melt <- melt(a, id.vars = 'Experiment' )

b <- ggplot(a_melt, aes(x=Experiment, y = value, group = variable, fill= variable))+
      geom_bar(stat = 'identity', colour='black',position = position_dodge(width = position_dodge_width))+
      theme_bw(base_size = 12)+
      ggtitle('Isotope Patterns detected and sequenced')+
      scale_fill_brewer(palette = palette)


if (long_names == TRUE) {
  b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
}  else{
  b
}

}

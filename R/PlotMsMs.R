#' Comparison of the MS/MS submmited and identified in each sample.
#'
#' @param summary The summary.txt table from  MaxQuant Output.
#' @param position_dodge_width Width overlapping columns.
#' @param font_size Size of the font in the labels.
#'
#' @return Plots the MS/MS submited and Identified in each sample.
#' @export
#'
#' @examples
PlotMsMs <- function(summary, position_dodge_width = 1, font_size=12){
  Experiment <- `MS/MS Submitted` <- `MS/MS Identified` <- value <- variable <- NULL

  a <- summary %>% select(c(Experiment, `MS/MS Submitted`, `MS/MS Identified`))
  a_melt <- melt(a, id.vars = 'Experiment' )


 ggplot(a_melt, aes(x=Experiment, y = value, group = variable, fill= variable))+
    geom_bar(stat = 'identity', colour='black',position = position_dodge(width = position_dodge_width))+
    theme_bw(base_size = font_size)+
    ggtitle('MS/MS Submitted and Identified')



# if (long_sample_name == TRUE) {
#   b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_sample_name,' ',x), width_sample_name))
#
# } else{
#   b
# }




}

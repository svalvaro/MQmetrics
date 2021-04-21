#' Comparison of the MS/MS submitted and identified in each sample.
#'
#' @param  summary The summary.txt table from  MaxQuant Output.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param position_dodge_width Position of the columns within each others.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Plots the MS/MS submitted and Identified in each sample.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' summary <- files[['summary.txt']]
#' PlotMsMs(summary)
#'
PlotMsMs <- function(summary,
                     long_names = FALSE,
                     sep_names = NULL,
                     position_dodge_width = 1,
                     palette = 'Set2'){

  Experiment <- `MS/MS Submitted` <- `MS/MS Identified` <- value <- variable <- NULL

  a <- summary %>% select(c(Experiment, `MS/MS Submitted`, `MS/MS Identified`))

  a_melt <- melt(a, id.vars = 'Experiment' )

 b <- ggplot(a_melt, aes(x=Experiment, y = value, group = variable, fill= variable))+
        geom_bar(stat = 'identity', colour='black',position = position_dodge(width = position_dodge_width))+
        theme_bw()+
        ggtitle('MS/MS Submitted and Identified')+
        scale_fill_brewer(palette = palette)+
        theme(legend.position = 'bottom')

if (long_names == TRUE) {
  b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))

} else{
  b
}
}

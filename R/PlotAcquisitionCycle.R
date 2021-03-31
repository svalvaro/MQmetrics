#' Acquisition Cycle and MS/MS
#'
#' @param msScans The msScans.txt file from the MaxQuant ouptut.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Two plots per sample, one with the cycle tyme vs retention time,
#'  and MS/MS count vs retention time.
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' msScans <- files[['msScans.txt']]
#' PlotAcquisitionCycle(msScans)
#'
PlotAcquisitionCycle <- function(msScans,
                                 palette = 'Set2'){
  data_table <- msScans %>%  select(contains(c('Experiment','Retention time',
                                               'Cycle time', 'MS/MS count' )))

  a <- ggplot(data_table, aes(x= `Retention time`, y = `Cycle time`, colour = Experiment))+
            geom_point(alpha = 0.5, show.legend = FALSE)+
            facet_wrap(.~ Experiment, ncol =1)+
            ggtitle('Cycle time')+
            theme_bw()+
            scale_colour_brewer(palette = palette)

  b <- ggplot(data_table, aes(x= `Retention time`, y = `MS/MS count`,colour = Experiment))+
              geom_point(alpha = 0.5, show.legend = FALSE)+
              facet_wrap(.~ Experiment, ncol =1)+
              ggtitle('MS/MS count')+
              theme_bw()+
              scale_colour_brewer(palette = palette)


  #Plot them together
  c <- plot_grid(a,b)
  #Make a title
  title <- ggdraw()+ draw_label('Acquisition Cycle')

  plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))

  }

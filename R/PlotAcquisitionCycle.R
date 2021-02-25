#' Title
#'
#' @param msScans
#'
#' @return
#' @export
#'
#' @examples
PlotAcquisitionCycle <- function(msScans){
  data_table <- msScans %>%  select(contains(c('Experiment','Retention time',
                                               'Cycle time', 'MS/MS count' )))

  a <- ggplot(data_table, aes(x= `Retention time`, y = `Cycle time`, colour = Experiment))+
            geom_point(alpha = 0.5, show.legend = FALSE)+
            facet_wrap(.~ Experiment, ncol =1)+
            ggtitle('Cycle time')+
            theme_bw()

  b <- ggplot(data_table, aes(x= `Retention time`, y = `MS/MS count`,colour = Experiment))+
              geom_point(alpha = 0.5, show.legend = FALSE)+
              facet_wrap(.~ Experiment, ncol =1)+
              ggtitle('MS/MS count')+
              theme_bw()


  #Plot them together
  c <- plot_grid(a,b)
  #Make a title
  title <- ggdraw()+ draw_label('Acquisition Cycle')

  plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))

  }

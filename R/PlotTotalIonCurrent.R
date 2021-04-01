#' Total Ion Current
#'
#' @param msmsScans The msmsScans.txt table from  MaxQuant Output.
#' @param show_max_value If TRUE, it will show the max TIC value of each sample.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Returns a plot the Total Ion Current in each sample. The maximum value is also plotted.
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' msmsScans <- files[['msmsScans.txt']]
#' PlotTotalIonCurrent(msmsScans)
#'
#'
PlotTotalIonCurrent <- function(msmsScans,
                                show_max_value = TRUE,
                                palette = 'Set2'){

  `Retention time` <- `Total ion current` <- Experiment <- . <- NULL

  a <- msmsScans %>%   ggplot(aes(`Retention time`,`Total ion current`))+
                            geom_line(aes(colour=Experiment))+
                            facet_wrap(.~ Experiment, ncol =1)+
                            ggtitle('Total Ion Current')+
                            theme_bw()+
                            theme(legend.position = 'none')+
                            scale_colour_brewer(palette = palette)


  if (show_max_value==TRUE) {
    a + geom_label(data = . %>% group_by(Experiment) %>% filter(`Total ion current`== max(`Total ion current`)),
                 aes(label= format(`Total ion current`, digits = 2, scientific = TRUE)), hjust=0.5)
  }else{
    a
  }

}


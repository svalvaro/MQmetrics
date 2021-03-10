#' Plot Total Ion Current
#'
#' @param msmsScans The msmsScans.txt table from  MaxQuant Output.
#'
#' @return Returns a plot the Total Ion Current in each sample. The maximum value is also plotted.
#' @export
#'
#' @examples
PlotTotalIonCurrent <- function(msmsScans, palette = 'Set2'){
  `Retention time` <- `Total ion current` <- Experiment <- . <- NULL

  msmsScans %>%   ggplot(aes(`Retention time`,`Total ion current`))+
    geom_line(aes(colour=Experiment))+
    geom_label(data = . %>% group_by(Experiment) %>% filter(`Total ion current`== max(`Total ion current`)),
               aes(label= format(`Total ion current`, digits = 2, scientific = TRUE)), hjust=0.5)+
    facet_wrap(.~ Experiment, ncol =1)+
    ggtitle('Total Ion Current')+
    theme_bw()+
    theme(legend.position = 'none')+
    scale_colour_brewer(palette = palette)

}


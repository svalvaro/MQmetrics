#' Intensity / LFQ intensity per sample.
#'
#' @param proteinGroups  The proteinGroups.txt table from  MaxQuant Output.
#' @param intensity_type The type of intensity. Values: 'Intensity', 'LFQ'.
#'
#' @return A violin plot of the intensities in each sample.
#' @export
#'
#' @examples
PlotIntensity <- function(proteinGroups, intensity_type = 'Intensity'){
  id <- variable <- value <- NULL

  if (intensity_type == 'Intensity') {
    intensities <-  proteinGroups %>%  select(id, contains('Intensity ')& -contains('LFQ'))
    title <- 'Intensity'
    ylab <-  expression('Log'[10]*'(Intensity)')
  }

  if (intensity_type == 'LFQ'){
    intensities <-  proteinGroups %>%  select(id, contains('LFQ intensity '))
    title <- 'LFQ intensity'
    ylab <-  expression('Log'[10]*'(LFQ intensity)')
  }



  intensities_measure <- colnames(intensities)
  intensities_measure <- intensities_measure[! intensities_measure %in% 'id']

  melted_intensities <- melt(log10(intensities), id.vars = 'id', measure.vars = intensities_measure)


  ggplot(melted_intensities, aes(x = variable, y = value, fill = variable))+
    geom_violin()+
    geom_boxplot(width=0.2)+
    ggtitle(label = title)+
    xlab('Experiment')+
    ylab(ylab)+
    theme(legend.position = 'none')



}

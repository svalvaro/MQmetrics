#' Intensity / LFQ intensity per sample.
#'
#' @param proteinGroups  The proteinGroups.txt table from  MaxQuant Output.
#' @param intensity_type The type of intensity. Values: 'Intensity', 'LFQ'.
#' @param  log_base The logarithmic scale for the intensity. Values: '10', '2', 'none'.
#'
#' @return A violin plot of the intensities in each sample.
#' @export
#'
#' @examples
PlotIntensity <- function(proteinGroups, intensity_type = 'Intensity', log_base = 10){
  id <- variable <- value <- NULL

  if (intensity_type == 'Intensity') {
    intensities <-  proteinGroups %>%  select(id, contains('Intensity ')& -contains('LFQ'))
    title <- 'Intensity'

  }

  if (intensity_type == 'LFQ'){
    intensities <-  proteinGroups %>%  select(id, contains('LFQ intensity '))
    title <- 'LFQ intensity'

  }



  intensities_measure <- colnames(intensities)
  intensities_measure <- intensities_measure[! intensities_measure %in% 'id']

  if(log_base == 2){
    melted_intensities <- melt(log2(intensities), id.vars = 'id', measure.vars = intensities_measure)


  }

  if (log_base == 10){
    melted_intensities <- melt(log10(intensities), id.vars = 'id', measure.vars = intensities_measure)

  }

  if (log_base == 'none'){
    melted_intensities <- melt(intensities, id.vars = 'id', measure.vars = intensities_measure)

  }

  #For the y_lab

  if(intensity_type == 'Intensity' & log_base == 2){
    ylab <- expression('Log'[2]*'(Intensity)')
  }
  if(intensity_type == 'Intensity' & log_base == 10){
    ylab <- expression('Log'[10]*'(Intensity)')
  }
  if(intensity_type == 'LFQ' & log_base == 2){
    ylab <- expression('Log'[2]*'(LFQ intensity)')
  }
  if(intensity_type == 'LFQ' & log_base == 10){
    ylab <- expression('Log'[10]*'(LFQ intensity)')
  }

  ggplot(melted_intensities, aes(x = variable, y = value, fill = variable))+
    geom_violin()+
    geom_boxplot(width=0.2)+
    ggtitle(label = title)+
    xlab('Experiment')+
    ylab(ylab)+
    theme(legend.position = 'none')



}

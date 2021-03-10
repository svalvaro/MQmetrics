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
PlotIntensity <- function(proteinGroups, intensity_type = 'Intensity', log_base = 10,
                          font_size = 12, long_names = FALSE, sep_names = NULL,
                          palette = 'Set2'){
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


  b <-   ggplot(melted_intensities, aes(x = variable, y = value, color = variable))+
            geom_violin(fill = 'gray80', size = 1, alpha = .5)+
            geom_boxplot(width=0.2)+
            ggtitle('Protein Intensity')+
            xlab('Experiment')+
            ylab(ylab)+
            theme_bw(base_size = font_size)+
            theme(legend.position = 'none')+
            scale_color_brewer(palette = palette)




if(long_names==TRUE){
  b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
} else{
  b
}




}

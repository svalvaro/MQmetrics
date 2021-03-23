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
PlotIntensity <- function(proteinGroups,
                          split_violin_intensity = TRUE,
                          intensity_type = 'Intensity',
                          log_base = 10,
                          long_names = FALSE,
                          sep_names = NULL,
                          palette = 'Set2'){



  GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                             draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                 stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                           1))
                                 quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                 aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                 aesthetics$alpha <- rep(1, nrow(quantiles))
                                 both <- cbind(quantiles, aesthetics)
                                 quantile_grob <- GeomPath$draw_panel(both, ...)
                                 ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                 ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                             })

  geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                                draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
  }



  id <- variable <- value <- NULL


  if(split_violin_intensity == TRUE){
    split_intensities <-  proteinGroups %>%  select(id, contains('Intensity '))

    #Error if no LFQ was found
    if(length(split_intensities %>% select(contains('LFQ')))== 0){
      print('LFQ intensities not found, split_violin_plot can not be created')
      print('Change the parameter split_violin_intensity to FALSE')
    } else{

      #create a table with the columns : id, sample, group, value
      df <-split_intensities %>%
             pivot_longer(-id,
                     names_to = c("intensity_type", "sample" ),
                     names_prefix = 'LFQ intensity',
                     names_sep = ' ',
                     values_to = "value")

      df$intensity_type[df$intensity_type == ''] = 'LFQ intensity'


     if (log_base == 10) {
       a <- ggplot(df, aes(sample, log10(value), fill = intensity_type))+
                   geom_split_violin()+
                   geom_boxplot(width=0.2, outlier.shape = NA)+
                   ggtitle('Protein Intensity & LFQ intensity')+
                   xlab('Experiment')+
                   ylab(expression('Log'[10]*'(Intensity)'))+
                   theme_bw()+
                   scale_fill_brewer(palette = palette)+
                   theme(legend.position = 'bottom')
     }

     if (log_base == 2) {
       a <- ggplot(df, aes(sample, log2(value), fill = intensity_type))+
                   geom_split_violin()+
                   geom_boxplot(width=0.2, outlier.shape = NA)+
                   ggtitle('Protein Intensity & LFQ intensity')+
                   xlab('Experiment')+
                   ylab(expression('Log'[2]*'(Intensity)'))+
                   theme_bw()+
                   scale_fill_brewer(palette = palette)+
                   theme(legend.position = 'bottom')
     }

    if(long_names==TRUE){
      a + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
    } else{
      a
    }
    }
  #if split_violin_plot == FALSE, Intensity or LFQ intensity will be plotted.
  } else{


      if (intensity_type == 'Intensity') {
        intensities <-  proteinGroups %>%  select(id, contains('Intensity ')& -contains('LFQ'))
        title <- 'Intensity'

      }

      if (intensity_type == 'LFQ'){
        intensities <-  proteinGroups %>%  select(id, contains('LFQ intensity '))
        title <- 'LFQ intensity'

        #Error if LFQ Intensity not found.

        if (length(intensities) == 1) {
          print('LFQ intensities not found, changing automatically to Intensity.')

          intensities <-  proteinGroups %>%  select(id, contains('Intensity ')& -contains('LFQ'))
          title <- 'Intensity'

        }

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
                geom_boxplot(width=0.2, outlier.shape = NA)+
                ggtitle('Protein Intensity')+
                xlab('Experiment')+
                ylab(ylab)+
                theme_bw()+
                theme(legend.position = 'none')+
                scale_color_brewer(palette = palette)




    if(long_names==TRUE){
      b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
    } else{
      b
    }

}


}

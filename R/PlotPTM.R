#' Post Translational Modifications
#'
#' @param modificationSpecificPeptides modificationSpecificPeptides table from MaxQuant ouput.
#' @param peptides_modified Minimum number of peptides modified. Default  is 5.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Two plots per sample
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' modificationSpecificPeptides <- files[["modificationSpecificPeptides.txt"]]
#' PlotPTM(modificationSpecificPeptides)
#'
PlotPTM <- function(modificationSpecificPeptides, peptides_modified = 5, palette = 'Set2'){

  modification_table <- modificationSpecificPeptides %>%
                          select(contains(c('Modifications', 'Proteins',
                                          'Intensity ', 'Experiment '))) %>%
                          select(-contains(c('calibrated', 'Unique (Proteins)')))

  mod_melted <- modification_table %>%
                    select(-contains('Intensity'))

  mod_melted <- melt(mod_melted, id.vars = c('Modifications','Proteins'))

  mod_melted[is.na(mod_melted)] <- 0

  mod_melted <- mod_melted[(mod_melted$value > peptides_modified),]


  mod_frequencies <- mod_melted %>%
                        group_by(Modifications, variable) %>%
                        summarise(Freq= sum(value))




  modifications_unique <- unique(mod_frequencies$Modifications)#This are the names
  #of the modifications that will be used for the other plot.

  a <- ggplot(mod_frequencies, aes(x = Modifications, y = Freq, fill = Modifications))+
          geom_bar(stat = 'identity')+
          facet_wrap(.~ variable, ncol =1)+
          theme_bw()+
          ylab('Frequency')+
          theme(legend.position = 'bottom',
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())+
          guides(fill = guide_legend(ncol=3))+
          scale_fill_brewer(palette =  palette)



  # Freq vs intensity

  # mod_intensities <- melt(modification_table, id.vars = c('Modifications',
  #                         'Proteins',
  #                         select(colnames(modification_table), contains('Experiment'))))
  mod_intensities <- modification_table %>%
                  select(-contains('Experiment'))

  mod_intensities <- melt(mod_intensities,
                          id.vars = c('Modifications','Proteins'))

  mod_intensities <- mod_intensities[mod_intensities$value !=0,]


  mod_intensities2 <- mod_intensities[mod_intensities$Modifications %in% modifications_unique,]

  b <- ggplot(mod_intensities2, aes(x = Modifications, y = log10(value), color = Modifications))+
            geom_violin(fill = 'gray80', size = 1, alpha = .5)+
            geom_boxplot(width=0.2)+
            facet_wrap(.~ variable, ncol =1)+
            theme_bw()+
            ylab(expression('Log'[10]*'Intensity'))+
            theme(legend.position = 'bottom',
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())+
            scale_color_brewer(palette = palette)


  c <- plot_grid( a+ theme(legend.position = 'none'),
                  b+theme(legend.position = 'none'),
                  ncol = 2, rel_heights=c(0.1, 1))
  title <- ggdraw()+draw_label('Post-Translational Modifications')

  prow <-  plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))

  legend <- get_legend(a)

  plot_grid(prow, legend, ncol = 1, rel_heights=c(peptides_modified, 1))
}

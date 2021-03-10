#' Title
#'
#' @param modificationSpecificPeptides
#'
#' @return
#' @export
#'
#' @examples
PlotPTM <- function(modificationSpecificPeptides, freq_min = 5){

  modification_table <- modificationSpecificPeptides %>%
                          select(contains(c('Modifications', 'Proteins',
                                          'Intensity ', 'Experiment '))) %>%
                          select(-contains(c('calibrated', 'Unique (Proteins)')))

  mod_melted <- modification_table %>%
                    select(-contains('Intensity'))

  mod_melted <- melt(mod_melted, id.vars = c('Modifications','Proteins'))

  mod_melted[is.na(mod_melted)] <- 0

  mod_melted <- mod_melted[(mod_melted$value > freq_min),]


  mod_frequencies <- mod_melted %>%
                        group_by(Modifications, variable) %>%
                        summarise(Freq= sum(value))




  modifications_unique <- unique(mod_frequencies$Modifications)#This are the names
  #of the modifications that will be used for the other plot.

  ggplot(mod_frequencies, aes(x = Modifications, y = Freq, fill = Modifications))+
          geom_bar(stat = 'identity')+
          facet_wrap(.~ variable, ncol =1)+
          theme_bw()+
          ylab('Frequency')+
          ggtitle('Post-Translational Modification')+
          theme(legend.position = 'bottom',
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())+
          guides(fill = guide_legend(ncol=2))



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

  ggplot(mod_intensities2, aes(x = Modifications, y = log10(value), color = Modifications))+
    geom_violin(fill = 'gray80', size = 1, alpha = .5)+
    geom_boxplot(width=0.2)+
    facet_wrap(.~ variable, ncol =1)+
    theme_bw()+
    ylab('Frequency')+
    ggtitle('Post-Translational Modification')+
    theme(legend.position = 'bottom',
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  # colnames(mod_intensities)[colnames(mod_intensities) == 'value'] <- 'Freq'
  # colnames(mod_intensities)[colnames(mod_intensities) == 'variable'] <- 'Experiment'
  #
  # mod_intensities2 <- melt(mod_intensities,
  #                          id.vars = c('Modifications','Proteins','Freq', 'Experiment'))
  #
  # mod_intensities2[is.na(mod_intensities2)] <- 0
  #
  # mod_intensities2 <- mod_intensities2[mod_intensities2$Freq !=0 | mod_intensities2$value!=0,]
}

#' Title
#'
#' @param modificationSpecificPeptides
#'
#' @return
#' @export
#'
#' @examples
PlotPTM <- function(modificationSpecificPeptides){
  modification_table <- modificationSpecificPeptides %>%
                          select(contains(c('Modifications', 'Proteins',
                                          'Intensity ', 'Retention time', 'Experiment '))) %>%
                          select(-contains(c('calibrated', 'Unique (Proteins)')))

  modification_melted <- melt(modification_table,
                              id.vars = c('Modifications','Proteins',
                                          'Retention time', select(colnames(modification_table),contains('Experiment '))))

  modification_frequencies <- modification_melted %>%
                                  group_by(variable,Modifications) %>%
                                  summarise(Freq=sum(value))


  modification_frequencies <- modification_melted %>%
                                    select(contains(c('Modifications', 'variable')))# %>%
                                    # group_by(variable, Modifications) %>%
                                    # summarise(Freq=n())

  modification_frequencies2 <- subset(as.data.frame(table(modification_frequencies)), Freq != 0)

  #To much information if plotted against retention time,

  # ggplot(modification_melted, aes(x = `Retention time`, y = value, colour = Modifications))+
  #   geom_bar(stat =  'identity')+
  #   facet_wrap(.~ variable, ncol =1)+
  #   theme(legend.position = 'bottom')

  #The best will be a frequency table, with each of the modification and the number
  #of appearences.



  ggplot(modification_frequencies, aes(x = Modifications, y = Freq))+
    geom_bar(stat = 'identity', na.rm = TRUE)+
    facet_wrap(.~ variable, ncol =1)+
    theme(legend.position = 'bottom', font = 4)

}

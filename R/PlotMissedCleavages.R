#' Missed Cleavages
#'
#' @return
#' @export
#'
#' @examples
PlotMissedCleavages <- function(peptides, position_dodge_width = 1){

  pep_melt <-  melt(peptides, id.vars ="Missed cleavages", measure.vars = colnames(peptides %>% select(contains('Experiment'))))
  pep_melt<- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)

  ggplot(pep_melt, aes(x = `Missed cleavages`  , y = value, fill = variable))+
    geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width))+
    #facet_grid(variable~.)+
    ggtitle(label = 'Number of missed enzymatic cleavages.')+
    #theme(legend.position = 'none')+
    xlab(label = 'Missed Cleavages')

}


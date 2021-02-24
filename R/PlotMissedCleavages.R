#' Missed Cleavages
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param  position_dodge_width position of the columns within each others.
#'
#' @return A plot showing the missed cleavages in each column.
#' @export
#'
#' @examples
PlotMissedCleavages <- function(peptides, font_size =12 , position_dodge_width = 1){
  `Missed cleavages` <- value <- variable <- NULL

  pep_melt <-  melt(peptides, id.vars ="Missed cleavages", measure.vars = colnames(peptides %>% select(contains('Experiment'))))
  pep_melt<- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)

  ggplot(pep_melt, aes(x = `Missed cleavages`  , y = value, fill = variable))+
    geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width))+
    #facet_grid(variable~.)+
    ggtitle(label = 'Number of missed enzymatic cleavages.')+
    #theme(legend.position = 'none')+
    xlab(label = 'Missed Cleavages')+
    theme_bw(base_size = font_size)



}


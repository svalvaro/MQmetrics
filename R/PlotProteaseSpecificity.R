#' Missed Cleavages
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param  position_dodge_width position of the columns within each others.
#'
#' @return A plot showing the missed cleavages in each column.
#' @export
#'
#' @examples
PlotProteaseSpecificity <- function(peptides, font_size =12 , position_dodge_width = 1){
  `Missed cleavages` <- value <- variable <- NULL

  peptides2 <- peptides %>%  select(contains(c('Missed cleavages', 'Experiment', 'Length')))

  pep_melt <-  melt(peptides2, id.vars =c("Missed cleavages", 'Length'), measure.vars = colnames(peptides %>% select(contains(c('Experiment')))))
  pep_melt1<- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)


  #create plot1 for missed cleavages
  plot_cleavages <- ggplot(pep_melt1, aes(x = `Missed cleavages`  , y = value, fill = variable))+
                          geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width),
                                   show.legend = FALSE)+
                          ggtitle(label = 'Missed enzymatic cleavages')+
                          facet_wrap(.~ variable, ncol =1)+
                          theme_bw(base_size = font_size)+
                          xlab(label = 'Missed Cleavages')

  #Create plot2 for length peptides
  pep_melt2 <-   pep_melt
  pep_melt2$value = 1
  pep_melt2 <- aggregate(value ~ variable + Length , data=pep_melt, sum)

 plot_length <- ggplot(pep_melt2, aes(x = Length, y = value, fill = variable ))     +
                     geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width),
                              show.legend = FALSE)+
                     ggtitle(label = 'Peptide Length')+
                     xlab(label = 'Length')+
                     facet_wrap(.~ variable, ncol =1)+
                     theme_bw(base_size = font_size)+
                     xlab(label = 'Peptide length')


 #Plot them together
 c <- plot_grid(plot_length, plot_cleavages)
 #Make a title
 title <- ggdraw()+ draw_label('Protease Specificity', size = 15)

 plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1.5))

}


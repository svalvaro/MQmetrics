#' Protease Specificity
#'
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param  position_dodge_width position of the columns within each others.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Two plots per sample: Peptide length distribution and the number of
#'  missed enzymatic cleavages.
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' PlotProteaseSpecificity(peptides)
#'
PlotProteaseSpecificity <- function(peptides,
                                    position_dodge_width = 1,
                                    palette = 'Set2'){

  `Missed cleavages` <- value <- variable <- Length  <- NULL

  peptides2 <- peptides %>%  select(contains(c('Missed cleavages', 'Experiment', 'Length')))

  pep_melt <-  melt(peptides2, id.vars =c("Missed cleavages", 'Length'), measure.vars = colnames(peptides %>% select(contains(c('Experiment')))))
  pep_melt1<- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)


  #Create plot2 for length peptides
  pep_melt2 <-   pep_melt
  pep_melt2$value = 1
  pep_melt2 <- aggregate(value ~ variable + Length , data=pep_melt, sum)



  n_samples <- length(peptides2)-2

  n_pages_needed <- ceiling(
    n_samples/ 5
  )

  colourCount = n_samples

  getPalette = colorRampPalette(brewer.pal(8, palette))

  for (ii in seq_len(n_pages_needed)) {

    if(n_samples <5){
      nrow = n_samples
    } else{
      nrow = 5
    }

    #create plot1 for missed cleavages
    plot_cleavages <- ggplot(pep_melt1, aes(x = `Missed cleavages`  , y = value, fill = variable))+
      geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width),
               show.legend = FALSE)+
      ggtitle(label = 'Missed enzymatic cleavages')+
      facet_wrap_paginate(.~ variable, ncol =1, nrow = nrow, page = ii)+
      theme_bw()+
      xlab(label = 'Missed Cleavages')+
      scale_fill_manual(values = getPalette(colourCount))




    plot_length <- ggplot(pep_melt2, aes(x = Length, y = value, fill = variable ))     +
      geom_bar(stat = 'identity', color = 'black', position = position_dodge(width = position_dodge_width),
               show.legend = FALSE)+
      ggtitle(label = 'Peptide Length')+
      xlab(label = 'Length')+
      facet_wrap_paginate(.~ variable, ncol =1, nrow = nrow, page = ii)+
      theme_bw()+
      xlab(label = 'Peptide length')+
      scale_fill_manual(values = getPalette(colourCount))




    #Plot them together
    c <- plot_grid(plot_length, plot_cleavages)
    #Make a title
    title <- ggdraw()+ draw_label('Protease Specificity', size = 15)

    print(plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1.5)))

  }






}


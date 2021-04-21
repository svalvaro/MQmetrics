#' Protease Specificity
#'
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#' @param plots_per_page Establish the maximum number of plots per page.
#'
#' @return Two plots per sample: Peptide length distribution and the number of
#'  missed enzymatic cleavages.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' PlotProteaseSpecificity(peptides)
#'
PlotProteaseSpecificity <- function(peptides,
                                    palette = 'Set2',
                                    plots_per_page = 5){

  `Missed cleavages` <- value <- variable <- Length  <- NULL

  peptides <- peptides %>%  select(contains(c('Missed cleavages', 'Experiment', 'Length')))

  pep_melt <-  melt(peptides, id.vars =c("Missed cleavages", 'Length'), measure.vars = colnames(peptides %>% select(contains(c('Experiment')))))
  pep_melt1<- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)

  pep_melt1$variable <- gsub('Experiment', '', pep_melt1$variable)

  #Create plot2 for length peptides
  pep_melt2 <-   pep_melt
  pep_melt2$value = 1
  pep_melt2 <- aggregate(value ~ variable + Length , data=pep_melt, sum)

  pep_melt2$variable <- gsub('Experiment', '', pep_melt2$variable)

  n_samples <- length(peptides)-2

  n_pages_needed <- ceiling(
    n_samples/ plots_per_page
  )

  colourCount = n_samples

  getPalette = colorRampPalette(brewer.pal(8, palette))

  for (ii in seq_len(n_pages_needed)) {

    if(n_samples < plots_per_page){
      nrow = n_samples
    } else{
      nrow = plots_per_page
    }

    #create plot1 for missed cleavages
    plot_cleavages <- ggplot(pep_melt1, aes(x = `Missed cleavages`  , y = value, fill = variable))+
      geom_bar(stat = 'identity', color = 'black',
               show.legend = FALSE)+
      ggtitle(label = 'Missed enzymatic cleavages')+
      facet_wrap_paginate(.~ variable, ncol =1, nrow = nrow, page = ii)+
      theme_bw()+
      xlab(label = 'Missed Cleavages')+
      scale_fill_manual(values = getPalette(colourCount))

    plot_length <- ggplot(pep_melt2, aes(x = Length, y = value, fill = variable ))     +
      geom_bar(stat = 'identity', color = 'black',
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


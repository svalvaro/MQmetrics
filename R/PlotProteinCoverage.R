#' Protein coverage and degradation
#'
#' @param peptides
#'
#' @return
#' @export
#'
#' @examples
PlotProteinCoverage <- function(peptides, UniprotID = 'A3KMP2'){
  table_peptides <- peptides %>% select(contains(c( 'Intensity', 'Start position',
                                                    'End position', 'Proteins'))) %>%
    select(-contains(c('LFQ', 'Unique'))) %>%  select(-'Intensity')

  #Select rows for the protein selected
  table_peptides <- table_peptides[(table_peptides$Proteins == UniprotID),]


  pep_melt <- melt(table_peptides, id.vars = c('Start position', 'End position', 'Proteins'))


  # If intensity is 0, remove it.

  pep_melt <- pep_melt[!pep_melt$value==0,]


  #Create a plot for the start vs end position
  a <- ggplot(pep_melt, aes(x = `Start position`, y = `End position`))+
          geom_point( alpha = 0.75)+
          theme_bw()+
          facet_wrap(.~ variable, ncol =1)


  #For the Intensity vs start position

  # prot_selected <- proteinGroups[(proteinGroups$`Protein IDs`)==UniprotID,]
  #
  # x_axis_length <- range(1:prot_selected$`Sequence length`)





  # #Plot for Intensity vs Start position
  #  b <- ggplot(pep_melt , aes(x = x_axis_length, y = log10(value)))+
  #           geom_point( alpha = 0.75)+
  #           theme_bw()+
  #           ylab(expression('Log'[10]*'(Intensity)'))+
  #           facet_wrap(.~ variable, ncol =1)

  #Plot for Intensity vs Start position
  b <- ggplot(pep_melt )+
            geom_segment(aes(x=`Start position`,
                             xend=`End position`,
                             y = log10(value),
                             yend =log10(value)))+
            theme_bw()+
            ylab(expression('Log'[10]*'(Intensity)'))+
            facet_wrap(.~ variable, ncol =1)



  #Plot them together
   c <- plot_grid(a,b)
   #Make a title
   title <- ggdraw()+ draw_label(paste0('Protein Coverage of: ', UniprotID))

   plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))



}

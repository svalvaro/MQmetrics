#' Protein coverage and degradation
#'
#' @param peptides
#'
#' @return
#' @export
#'
#' @examples
PlotProteinCoverage <- function(peptides, UniprotID = 'A0A286YCV9', log_base = 2, segment_width =1 ){


  table_peptides <- peptides %>% select(contains(c( 'Intensity', 'Start position',
                                                    'End position', 'Proteins'))) %>%
    select(-contains(c('LFQ', 'Unique'))) %>%  select(-'Intensity')

  #Select rows for the protein selected
  table_peptides <- table_peptides[grepl(UniprotID, table_peptides$Proteins ),]


  #Protein Length

  # prot_length <- proteinGroups[grepl(UniprotID, proteinGroups$`Protein IDs` ),]
  # prot_length <- proteinGroups[1,]
  # prot_length <- proteinGroups$`Sequence length`[1]


  #table_peptides <- table_peptides[1,]

  pep_melt <- melt(table_peptides, id.vars = c('Start position', 'End position', 'Proteins'))


  # If intensity is 0, remove it.

  pep_melt <- pep_melt[!pep_melt$value==0,]


  #Create a plot for the start vs end position
  # a <- ggplot(pep_melt, aes(x = `Start position`, y = `End position`,
  #                           colour = variable))+
  #         geom_point( alpha = 0.75)+
  #         theme_bw()+
  #         facet_wrap(.~ variable, ncol =1)+
  #         #scale_x_continuous(limits = c(1, prot_length))+
  #         theme(legend.position = 'none')
  a <- ggplot(pep_melt)+
            geom_segment(aes(x = `Start position`,
                             xend = `End position`,
                             y = `Start position`,
                             yend = `End position`,
                             colour = variable),
                             size = segment_width)+
            theme_bw()+
            facet_wrap(.~ variable, ncol =1)+
            ylab('End position')+
            #scale_x_continuous(limits = c(1, prot_length))+
            theme(legend.position = 'none')



  #Create a plot for the protein lenght vs the coverage

  if(log_base == 10){
    b <- ggplot(pep_melt )+
              geom_segment(aes(x=`Start position`,
                               xend=`End position`,
                               y = log10(value),
                               yend =log10(value),
                               colour = variable), size = segment_width )+
              theme_bw()+
              ylab(expression('Log'[10]*'(Intensity)'))+
              facet_wrap(.~ variable, ncol =1)+
              #scale_x_continuous(limits = c(1, prot_length))+
              theme(legend.position = 'none')

  } else{
    b <- ggplot(pep_melt )+
              geom_segment(aes(x=`Start position`,
                               xend=`End position`,
                               y = log2(value),
                               yend =log2(value),
                               colour = variable),size = segment_width )+
              theme_bw()+
              ylab(expression('Log'[2]*'(Intensity)'))+
              facet_wrap(.~ variable, ncol =1)+
              #scale_x_continuous(limits = c(1, prot_length))+
              theme(legend.position = 'none')
  }





  #Plot them together
   c <- plot_grid(a,b)
   #Make a title
   title <- ggdraw()+ draw_label(paste0('Protein Coverage of: ', UniprotID))

   plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))



}

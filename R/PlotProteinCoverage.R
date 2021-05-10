#' Protein coverage and degradation.
#'
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param proteinGroups proteinGroups.txt table from MaxQuant output.
#' @param UniprotID Uniprot ID of the protein of interest.
#' @param log_base The logarithmic scale for the intensity. Default is 2.
#' @param segment_width Width of the segments to improve visualization.
#' Default is 1.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#' @param plots_per_page Establish the maximum number of plots per page.
#'
#' @return Two plots for each sample, the end position vs the start position of
#'  each peptide of the given protein found. And the Intensity of a given
#'  peptide and its length.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotProteinCoverage(peptides, proteinGroups, UniprotID = 'Q8R0Y6')
#'
PlotProteinCoverage <- function(peptides,
                                proteinGroups,
                                UniprotID = NULL,
                                log_base = 2,
                                segment_width = 1,
                                palette = 'Set2',
                                plots_per_page = 5){

  `Start position` <-  `End position` <- variable <- value <- NULL

  table_peptides <- peptides %>%
    select(contains(c('Intensity ', 'Start position',
                      'End position', 'Proteins', 'Gene names')))%>%
    select(-contains('Unique')) %>%
    select(-starts_with('LFQ'))# %>%
  #select(-'Intensity')

  #Select rows for the protein selected
  table_peptides <- table_peptides[grepl(UniprotID, table_peptides$Proteins ),]

  if(nrow(table_peptides) == 0){
    print(paste0('The protein: ',
                 UniprotID ,
                 ' provided was not identified in any of the samples.'))
  } else{

    #Total protein coverage

    prot_info <- proteinGroups[grepl(UniprotID, proteinGroups$`Protein IDs` ),]
    prot_cov <- prot_info$`Sequence coverage [%]`[1]

    prot_len <- prot_info$`Sequence length`[1]


    #table_peptides <- table_peptides[1,]

    pep_melt <- melt(table_peptides, id.vars = c('Start position',
                                                 'End position',
                                                 'Proteins',
                                                 'Gene names'))

    # If intensity is 0, remove it.

    pep_melt <- pep_melt[!pep_melt$value==0,]

    pep_melt$variable <- gsub('Intensity', '', pep_melt$variable)


    colourCount = length(unique(pep_melt$variable))

    getPalette = colorRampPalette(brewer.pal(8, palette))

    n_pages_needed <- ceiling(
      colourCount/ plots_per_page
    )

    for (ii in seq_len(n_pages_needed)) {

      if(colourCount <plots_per_page){
        nrow = colourCount
      } else{
        nrow = plots_per_page
      }

      a <- ggplot(pep_melt)+
        geom_segment(aes(x = `Start position`,
                         xend = `End position`,
                         y = `Start position`,
                         yend = `End position`,
                         colour = variable),
                     size = segment_width)+
        theme_bw()+
        facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow, page = ii)+
        ylab('End position')+
        theme(legend.position = 'none')+
        scale_color_manual(values = getPalette(colourCount))

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
          facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow, page = ii)+
          #scale_x_continuous(limits = c(1, prot_length))+
          theme(legend.position = 'none')+
          scale_color_manual(values = getPalette(colourCount))

      } else{
        b <- ggplot(pep_melt )+
          geom_segment(aes(x=`Start position`,
                           xend=`End position`,
                           y = log2(value),
                           yend =log2(value),
                           colour = variable),size = segment_width )+
          theme_bw()+
          ylab(expression('Log'[2]*'(Intensity)'))+
          facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow, page = ii)+
          #scale_x_continuous(limits = c(1, prot_length))+
          theme(legend.position = 'none')+
          scale_color_manual(values = getPalette(colourCount))
      }

      #Plot them together
      c <- plot_grid(a,b)
      #Make a title
      title <- ggdraw()+ draw_label(paste0('The Protein Coverage of: ', UniprotID, ' (',prot_len,' amino acids)',
                                           ', Gene: ', pep_melt$`Gene names`[1],
                                           '\n is: ', prot_cov, '%'))

      print(plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1)))

    }
  }
}

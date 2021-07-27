#' Protein coverage and degradation.
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
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
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotProteinCoverage(MQCombined, UniprotID = 'Q15149')
#'
PlotProteinCoverage <- function(MQCombined,
                                UniprotID = NULL,
                                log_base = 2,
                                segment_width = 1,
                                palette = 'Set2',
                                plots_per_page = 5){

    peptides <- MQCombined$peptides.txt
    proteinGroups <- MQCombined$proteinGroups.txt

    `Start position` <-  `End position` <- variable <- value <- NULL

    table_peptides <- peptides %>%
        select(contains(c('Intensity ', 'Start position',
                        'End position', 'Proteins', 'Gene names', 'Length'))
               )%>%
        select(-contains('Unique')) %>%
        select(-starts_with('LFQ'))# %>%
    #select(-'Intensity')

    #Select rows for the protein selected
    table_peptides <- table_peptides[grepl(UniprotID,
                                        table_peptides$Proteins ),]

    if(nrow(table_peptides) == 0){
        print(paste0('The protein: ',
                    UniprotID ,
                    ' provided was not identified in any of the samples.'))
    } else{

        #Total protein coverage

        prot_info <- proteinGroups[grepl(UniprotID,
                                        proteinGroups$`Protein IDs` ),]

        prot_cov <- prot_info$`Sequence coverage [%]`[1] # Protein Coverage

        prot_len <- prot_info$`Sequence length`[1] # Protein length




        pep_melt <- melt(table_peptides, id.vars = c('Start position',
                                                    'End position',
                                                    'Proteins',
                                                    'Gene names', 'Length'))

        # If intensity is 0, remove it.

        pep_melt <- pep_melt[!pep_melt$value==0,]

        pep_melt$variable <- gsub('Intensity', '', pep_melt$variable)



        # Calculate Individual's protein Coverage for each sample.

        # Right now it does not take into account the possible overlaps.

        individual_coverage <- pep_melt %>% select(contains(c('Position',
                                                              'Length',
                                                              'variable'))) %>%
            group_by(variable) %>%
            summarise(coverage = (sum(Length)/prot_len)*100)

        individual_coverage$coverage <- format(round(
            individual_coverage$coverage , 1), nsmall = 1)

        colourCount = length(unique(pep_melt$variable))

        getPalette = colorRampPalette(brewer.pal(8, palette))

        n_pages_needed <- ceiling(
            colourCount/ plots_per_page
        )


        myplots <- list()

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

                geom_text(individual_coverage,
                          mapping =  aes(
                              x = (max(pep_melt$`End position`) - min(pep_melt$`Start position`)) * 0.15,
                              y = max(pep_melt$`End position`) * 0.9,
                              label = paste0(coverage, ' % Prot. Coverage')))+
                theme_bw()+
                facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow,
                                    page = ii)+
                ylab('End position')+
                theme(legend.position = 'none')+
                scale_color_manual(values = getPalette(colourCount))

             #
             #
             #
             #
             # ggplot(pep_melt)+
             #    geom_segment(aes(x = `Start position`,
             #                    xend = `End position`,
             #                    y = `Start position`,
             #                    yend = `End position`,
             #                    colour = variable),
             #                    size = segment_width)+
             #    geom_text(individual_coverage,
             #                mapping =  aes(
             #                x = min(pep_melt$`Start position`) * 1.1,
             #                y = max(pep_melt$`End position`) * 0.9,
             #                label = paste0(coverage, ' % Prot. Coverage')))+
             #    theme_bw()+
             #    facet_wrap_paginate(.~ variable, ncol = 1, nrow = 6,
             #                        page = 1)+
             #    ylab('End position')+
             #    theme(legend.position = 'none')+
             #    scale_color_manual(values = getPalette(colourCount))

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
                    facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow,
                                        page = ii)+
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
                    facet_wrap_paginate(.~ variable, ncol = 1, nrow = nrow,
                                        page = ii)+
                    #scale_x_continuous(limits = c(1, prot_length))+
                    theme(legend.position = 'none')+
                    scale_color_manual(values = getPalette(colourCount))
            }

            #Plot them together
            c <- plot_grid(a,b)
            #Make a title
            title <- ggdraw()+ draw_label(paste0('The Protein Coverage of: ',
                                                UniprotID,
                                                ' (',prot_len,' amino acids)',
                                                ',\n Gene: ',
                                                pep_melt$`Gene names`[1],
                                                ' is: ', prot_cov, '%'))

            myplots[[ii]] <- plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))
            #p <- plot_grid( title, c, ncol = 1, rel_heights=c(0.1, 1))

        }

        return(myplots)
    }

}

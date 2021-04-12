#' Post Translational Modifications
#'
#' @param modificationSpecificPeptides modificationSpecificPeptides table from MaxQuant ouput.
#' @param peptides_modified Minimum number of peptides modified. Default  is 5.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Two plots per sample
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' modificationSpecificPeptides <- files[["modificationSpecificPeptides.txt"]]
#' PlotPTM(modificationSpecificPeptides)
#'
PlotPTM <- function(modificationSpecificPeptides,
                    peptides_modified = 1,
                    plot_unmodified_peptides = FALSE,
                    log_base = 2,
                    palette = 'Set2'){

  Modifications <- variable <- value <- Freq <- NULL

  modification_table <- modificationSpecificPeptides %>%
                          select(contains(c('Modifications', 'Proteins',
                                          'Intensity ', 'Experiment '))) %>%
                          select(-contains(c('calibrated', 'Unique (Proteins)')))


  mod_melted <- modification_table %>%
                    select(-contains('Intensity'))

  mod_melted <- melt(mod_melted, id.vars = c('Modifications','Proteins'))

  mod_melted[is.na(mod_melted)] <- 0

  #Min peptides modified per Protein, change it to per group.
  #mod_melted <- mod_melted[(mod_melted$value > peptides_modified),]

  mod_frequencies <- mod_melted %>%
                        group_by(Modifications, variable) %>%
                        summarise(Freq= sum(value))


  mod_join <- mod_frequencies %>% separate_rows(Modifications, sep = ';') %>%
            group_by(Modifications, variable) %>% summarise(Freq = sum(Freq))

  #Remove the Experiment pattern from the variable

  mod_join$variable <- gsub('Experiment', '', mod_join$variable)

  mod_join <- mod_join[mod_join$Freq >=  peptides_modified,]


  if (plot_unmodified_peptides == FALSE) {
    mod_join <- mod_join[!mod_join$Modifications == 'Unmodified',]

  } else{
    mod_join <- mod_join
  }


  #Combine multiple oxidation into the same group

  mod_join_combined <- mod_join

  #Multiply the freq by number of oxidations
  mod_join_combined$Freq[mod_join_combined$Modifications == '2 Oxidation (M)'] <- mod_join_combined$Freq[mod_join_combined$Modifications == '2 Oxidation (M)']*2

  mod_join_combined$Freq[mod_join_combined$Modifications == '2 Oxidation (M)'] <- mod_join_combined$Freq[mod_join_combined$Modifications == '3 Oxidation (M)']*3

  mod_join_combined$Freq[mod_join_combined$Modifications == '4 Oxidation (M)'] <- mod_join_combined$Freq[mod_join_combined$Modifications == '4 Oxidation (M)']*4

  mod_join_combined$Freq[mod_join_combined$Modifications == '5 Oxidation (M)'] <- mod_join_combined$Freq[mod_join_combined$Modifications == '5 Oxidation (M)']*5



  combined_oxidations <- aggregate(x = mod_join_combined[grep('Oxidation',mod_join_combined$Modifications),3],
                                  by = mod_join_combined[grep('Oxidation',mod_join_combined$Modifications),2],
                                  FUN = sum)

  #Remove rows with the oxidation columns
  mod_join_combined2 <- mod_join_combined
  mod_join_combined2 <- mod_join_combined2[!(mod_join_combined2$Modifications %in% c('2 Oxidation (M)',
                                                                                     '3 Oxidation (M)',
                                                                                     '4 Oxidation (M)',
                                                                                     '5 Oxidation (M)')),]

  #Change the values of the original oxidation to the sum of all the combined oxidations

  mod_join_combined2$Freq[mod_join_combined2$Modifications == 'Oxidation (M)'] <- NA

  combined_oxidations$Modifications <- 'Oxidation (M)'
  combined_oxidations <- combined_oxidations[,c(3,1,2)]

  modification_final <- full_join(mod_join_combined2, combined_oxidations, by = c("variable", 'Modifications'))

  modification_final$Freq <- coalesce(modification_final$Freq.x, modification_final$Freq.y)

  modification_final$Freq.x <- NULL
  modification_final$Freq.y <- NULL  #Probably there is an easier way to combine all the oxidations.

  ###############For Intensity plot


  modifications_unique <- unique(modification_final$Modifications)#This are the names

  mod_intensities <- modification_table %>%
    select(-contains('Experiment'))

  mod_intensities <- melt(mod_intensities,
                          id.vars = c('Modifications','Proteins'))

  mod_intensities <- mod_intensities[mod_intensities$value !=0,]

  #Select only the same modifications as in the Frequency
  mod_intensities2 <- mod_intensities[mod_intensities$Modifications %in% modifications_unique,]


  mod_intensities2$variable <- gsub('Intensity', '', mod_intensities2$variable)


  if (log_base == 2) {

    mod_intensities2$value <- log2(mod_intensities2$value)
    ylab = expression('Log'[2]*'(Intensity)')

  } else if (log_base == 10) {

    mod_intensities2$value <- log10(mod_intensities2$value)
    ylab = expression('Log'[10]*'(Intensity)')

  }

  #samples for paginate

  n_samples <- length(modification_table %>%  select(contains('Experiment')))

  n_pages_needed <- ceiling(
    n_samples/ 5
  )


  colourCount = n_samples

  getPalette = colorRampPalette(brewer.pal(8, palette))

  for (ii  in seq_len(n_pages_needed)) {

    if(n_samples < 5){
      nrow = n_samples
    } else{
      nrow = 5
    }


    a <- ggplot(modification_final, aes(x = Modifications, y = Freq, fill = Modifications))+
            geom_bar(stat = 'identity')+
            facet_wrap_paginate(.~ variable, ncol =1, nrow = nrow, page = ii)+
            theme_bw()+
            ggtitle('Frequency of modified peptides')+
            ylab('Frequency')+
            theme(legend.position = 'bottom',
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())+
                  guides(fill = guide_legend(ncol=3))+
                  scale_fill_brewer(palette = palette)



    b <- ggplot(mod_intensities2, aes(x = Modifications, y = value, color = Modifications))+
              geom_violin(fill = 'gray80', size = 1, alpha = .5)+
              geom_boxplot(width=0.2)+
              facet_wrap_paginate(.~ variable, ncol =1, nrow = nrow, page = ii)+
              theme_bw()+
              ggtitle('Intensities of modified peptides')+
              ylab(ylab)+
              theme(legend.position = 'bottom',
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())+
              scale_colour_brewer(palette = palette)



    c <- plot_grid( a+ theme(legend.position = 'none'),
                    b+theme(legend.position = 'none'),
                    ncol = 2, rel_heights=c(0.1, 1))
    title <- ggdraw()+draw_label('Post-Translational Modifications')

    prow <-  plot_grid(title, c, ncol = 1, rel_heights=c(0.1, 1))

    legend <- get_legend(a)

    d <- plot_grid(prow, legend, ncol = 1, rel_heights=c(9, 1))

    print(d)

  }

}

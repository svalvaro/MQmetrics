#' Max intensities of the iRT peptides in each sample.
#'
#' @param evidence evidence.txt table from the MaxQuant ouptut.
#'
#' @return A plot showing the iRT peptide in each sample vs the Retention time.
#' @export
#'
#' @examples
PlotiRT <- function(evidence, tolerance=0.001, font_size=12){

  Experiment <- `m/z` <- `Retention time` <- Sequence <- Intensity <- NULL

  iRT.mZ <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
              683.8282, 683.8541, 699.3388, 726.8361, 776.9301)


  names(iRT.mZ) <- iRT_sequences <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                     "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                     "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                     "LFLQFGAQGSPFLK")


  #Check for the iRT peptides by sequence
  indexes_prot <-  which(evidence$Sequence %in% iRT_sequences)



  #if no iRT peptide found return error.
  if (length(indexes_prot)==0) {

    print('No iRT peptides found in the MaxQuant output.')

  } else{

  #obtain rows only with irt by sequence
  iRT_table_prot <- evidence[indexes_prot,]

  #remove rows with NA in intensity
  iRT_table_prot <- iRT_table_prot[complete.cases(iRT_table_prot$Intensity),]

  #make table smaller
  iRT_table_prot <- iRT_table_prot %>% select(c(Experiment,`m/z`,`Retention time`,
                                               Sequence, Intensity))

  #from the irt obtained, filter them by the theoretical m/z with tolerance
  in_range <- unlist(sapply(iRT_table_prot$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)]))

  #Obtain the indexes and final table
  indexes <- which(iRT_table_prot$`m/z` %in% in_range)


  iRT_table_prot_final  <- iRT_table_prot[indexes,]

  #obtain the maximum intensity values for each experiment, and sequence.
  iRT_table_prot_maxvalues <- iRT_table_prot_final %>%
                                 group_by(Experiment, Sequence) %>%
                                 filter(Intensity
                                        == max(Intensity))


  ggplot(iRT_table_prot_maxvalues,aes(x = `Retention time`,
                                      y = Intensity,
                                      colour = Sequence))+
                                      # colour = as.character(`m/z`)))+
    geom_point()+
    geom_segment(aes(xend=`Retention time`, yend=0))+
    facet_wrap(. ~ Experiment, ncol= 1)+
    ggtitle('Biognosys iRT peptides in each sample.')+
    theme_bw(base_size = font_size)+
    #labs(colour='iRT peptides m/z')+
    theme(legend.position = 'bottom')


}
}

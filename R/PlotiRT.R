#' Title
#'
#' @param msmsScans
#'
#' @return
#' @export
#'
#' @examples
PlotiRT <- function(evidence){


  iRT.mZ <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
              683.8282, 683.8541, 699.3388, 726.8361, 776.9301)

  iRT.score <- c(-24.92, 19.79, 70.52, 87.23, 0, 28.71, 12.39, 33.38, 42.26,
                 54.62, 100)

  names(iRT.mZ) <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                     "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                     "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                     "LFLQFGAQGSPFLK")

  iRT_sequences <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                     "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                     "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                     "LFLQFGAQGSPFLK")


#
#   tolerance <- 0.001
#
#   in_range <- unlist(sapply(msmsScans$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)]))
#
#   indexes <- which(msmsScans$`m/z` %in% in_range)
#
#   iRT_table <- msmsScans[indexes,]
#
#   iRT_table <- iRT_table %>% select(c(Experiment,`m/z`,`Retention time`,
#                                       `Total ion current`, Sequence))


  #inrange with peptide names

  indexes_prot <-  which(evidence$Sequence %in% iRT_sequences)

  iRT_table_prot <- evidence[indexes_prot,]

  iRT_table_prot <- iRT_table_prot %>% select(c(Experiment,`m/z`,`Retention time`,
                                               Sequence, Intensity))
  iRT_table_prot_maxvalues <- iRT_table_prot %>%
                                 group_by(Experiment, Sequence) %>%
                                 filter(Intensity
                                        == max(Intensity))

  # ggplot(msmsScans, aes(x = `Retention time`, y = `Total ion current`))+
  #   geom_line(color= 'grey')+
  #   facet_grid(Experiment ~ .)+
  #   geom_line(iRT_table, mapping = aes(x = `Retention time`, y = `Total ion current`,
  #                            colour = Experiment))

  # ggplot(iRT_table_prot, mapping = aes(x = `Retention time`,
  #                                      y = `Precursor intensity`,
  #                                      colour = Sequence))+
  #         geom_point()+
  #         facet_grid(Experiment ~ .)+
  #         theme(legend.position = 'none')



  ggplot(iRT_table_prot_maxvalues,aes(x = `Retention time`,
                                      y = Intensity,
                                      colour = as.character(`m/z`)))+
    geom_point()+
    geom_segment(aes(xend=`Retention time`, yend=0))+
    facet_grid(Experiment ~ .)+
    theme(legend.position = 'bottom')


  # tolerance <- 0.001
  #
  # msmsScans[which(msmsScans$`m/z` == 354.89)
  #
  #
  # C <- sapply(msmsScans$`, function(x) which.min(abs(x-iRT.mZ)))
  # C <- C[match(in_range, A)]
  #
  # ggplot(msmsScans, aes(x = `Retention time`, y = `Total ion current`))+
  #   geom_line(aes(colour = Experiment))



}

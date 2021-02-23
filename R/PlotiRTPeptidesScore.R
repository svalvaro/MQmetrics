#' Title
#'
#' @param msmsScans
#'
#' @return
#' @export
#'
#' @examples
PlotiRTScore <- function(msmsScans){

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



    tolerance <- 0.001

    in_range <- unlist(sapply(msmsScans$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)]))

    indexes <- which(msmsScans$`m/z` %in% in_range)

    iRT_table <- msmsScans[indexes,]

    iRT_table <- iRT_table %>% select(c(Experiment,`m/z`,`Retention time`,
                                        `Total ion current`, Sequence, `Base peak intensity`))

    iRT_table_mz <- iRT_table %>%
      group_by(Experiment, Sequence) %>%
      filter(`Base peak intensity`==max(`Base peak intensity`))





  #   #inrange with peptide names
  #
  # indexes_prot <-  which(msmsScans$Sequence %in% iRT_sequences)
  #
  # iRT_table_prot <- msmsScans[indexes_prot,]
  #
  # iRT_table_prot <- iRT_table_prot %>% select(c(Experiment,`m/z`,`Retention time`,
  #                                               `Total ion current`, Sequence,
  #                                               `Precursor intensity`, `Base peak intensity`))
  # iRT_table_prot_maxvalues <- iRT_table_prot %>%
  #   group_by(Experiment, Sequence) %>%
  #   filter(`Base peak intensity`== max(`Base peak intensity`))
  #
  #
  #



}

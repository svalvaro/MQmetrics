#' Title
#'
#' @param MQPathCombined
#' @param log_base
#' @param intensity_type
#'
#' @return
#' @export
#'
#' @examples
ReportTables <- function(MQPathCombined, log_base = 2, intensity_type = 'Intensity'){

  files <- MQmetrics::ReadDataFromDir(MQPathCombined)

  #Proteins Identified and NAs
  proteinGroups <- files[["proteinGroups.txt"]]

  if (intensity_type == 'Intensity') {
    protein_table <- proteinGroups[,grep("Intensity ", colnames(proteinGroups))]
    #Remove Intensity from name
    colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))

    title <- 'Proteins Identified based on Intensity'

  }

  if (intensity_type == 'LFQ'){
    protein_table<- proteinGroups[,grep("LFQ", colnames(proteinGroups))]
    #Remove LFQ Intensity from name
    colnames(protein_table) <- gsub("LFQ intensity.", "", colnames(protein_table))
    title <- 'Proteins Identified based on LFQ intensity'



    #Error if LFQ Intensity not found.

    if (length(protein_table) == 0) {
      print('LFQ intensities not found, changing automatically to Intensity.')

      protein_table <- proteinGroups[,grep("Intensity ", colnames(proteinGroups))]
      #Remove Intensity from name
      colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))

      title <- 'Proteins Identified based on Intensity'

    }

  }


  # Table 1 Proteins Identified
  table_proteins <- data.frame(nrow(protein_table)-colSums(protein_table==0))

  rownames_prot <- rownames(table_proteins)

  table_proteins <- cbind(rownames_prot,table_proteins)
  #Order it
  table_proteins <- table_proteins[order(rownames(table_proteins)),]

  #Changing names
  colnames(table_proteins)[1] <- 'Experiment'
  colnames(table_proteins)[2] <- 'Proteins identified'

  #NAs
  table_proteins$'Missing values' <- nrow(protein_table) - table_proteins$`Proteins identified`


  rownames(table_proteins) <- NULL


  table_proteins <- table_proteins[, c('Experiment', 'Missing values', 'Proteins identified')]

  #table_proteins <- kable(table_proteins) #%>%
                      #kable_styling(position = "center")





  #Table 2 Log10 intensities


  int_info <- proteinGroups %>%  select(contains("Intensity ")) %>% select(-contains('LFQ'))

  int_info[int_info == 0] <- NA

  #log10 intensities
  dynamic_table <- do.call(data.frame,
                           list(mean = format(log10(apply(int_info, 2, mean,na.rm=TRUE)), digits = 4),
                                sd = format(log10(apply(int_info, 2, sd,na.rm=TRUE)), digits = 4),
                                median = format(log10(apply(int_info, 2, median,na.rm=TRUE)), digits =4),
                                min = format(log10(apply(int_info, 2, min,na.rm=TRUE)), digits = 4),
                                max = format(log10(apply(int_info, 2, max,na.rm=TRUE)), digits = 4),
                                n = apply(int_info, 2, length)-colSums(is.na(int_info))))

  rownames(dynamic_table) <- gsub('Intensity', 'Log10 Intensity', rownames(dynamic_table))


  #expression('Information of the log'[10]*'(Intensity)'))
  #dynamic_table <- kable(dynamic_table) #%>%
                        #kable_styling(position = "center")



  #Table 3 Charge

  evidence <- files[["evidence.txt"]]

  charge_table <- evidence %>%
                      select(c(Experiment,Charge ))

  charge_table <- dcast(charge_table, Experiment~ Charge, fill = 0)

  charge_percentage <- charge_table[,-1]/rowSums(charge_table[,-1])*100

  charge_percentage <- cbind(charge_table$Experiment, format(round(charge_percentage,1), nsmall =1))

  names(charge_percentage)[1] <- 'Experiment'

  #charge_percentage <- kable(charge_percentage)



   out <- list()

  out$proteins <- table_proteins
  out$intensities <- dynamic_table
  out$charge <- charge_percentage



  return(out)


}

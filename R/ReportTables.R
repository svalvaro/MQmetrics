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


  #Proteins Identified
  table_prot <- data.frame(nrow(protein_table)-colSums(protein_table==0))

  rownames_prot <- rownames(table_prot)

  table_proteins <- cbind(rownames_prot,table_prot)
  #Order it
  table_proteins <- table_proteins[order(rownames(table_proteins)),]

  #Changing names
  colnames(table_proteins)[1] <- 'Experiment'
  colnames(table_proteins)[2] <- 'Proteins identified'

  #NAs
  table_proteins$'Missing values' <- nrow(protein_table) - table_proteins$`Proteins identified`


  rownames(table_proteins) <- NULL


  table_proteins <- table_proteins[, c('Experiment', 'Missing values', 'Proteins identified')]

  table_proteins <- kable(table_proteins)#%>%
                      #kable_styling(position = "center")
  return(table_proteins)


}

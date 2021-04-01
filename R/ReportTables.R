#' Report Tables with summary data
#'
#' @param MQPathCombined The directory to the "combined" folder where the MaxQuant results are stored.
#' @param log_base The logarithmic scale for the intensity. Default is 2.
#' @param intensity_type The type of intensity. Values: 'Intensity' or 'LFQ'.
#'
#'
#' @return A list with four tables are generated:
#'  - Protein Information
#'  - Intensity Information
#'  - Peptide Charge Information
#'  - Peptide hydrophobicity Information
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' ReportTables(MQPathCombined)
#'
ReportTables <- function(MQPathCombined,
                         log_base = 2,
                         intensity_type = 'Intensity'){

  sd <- median <- Experiment <- Charge <- variable <- NULL

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

  if(log_base == 2){
    dynamic_table <- do.call(data.frame,
                             list(mean = format(log2(apply(int_info, 2, mean,na.rm=TRUE)), digits = 4),
                                  sd = format(log2(apply(int_info, 2, sd,na.rm=TRUE)), digits = 4),
                                  median = format(log2(apply(int_info, 2, median,na.rm=TRUE)), digits =4),
                                  min = format(log2(apply(int_info, 2, min,na.rm=TRUE)), digits = 4),
                                  max = format(log2(apply(int_info, 2, max,na.rm=TRUE)), digits = 4),
                                  n = apply(int_info, 2, length)-colSums(is.na(int_info))))

    rownames(dynamic_table) <- gsub('Intensity', 'Log2 Intensity', rownames(dynamic_table))
  }

  if(log_base == 10){
    dynamic_table <- do.call(data.frame,
                             list(mean = format(log10(apply(int_info, 2, mean,na.rm=TRUE)), digits = 4),
                                  sd = format(log10(apply(int_info, 2, sd,na.rm=TRUE)), digits = 4),
                                  median = format(log10(apply(int_info, 2, median,na.rm=TRUE)), digits =4),
                                  min = format(log10(apply(int_info, 2, min,na.rm=TRUE)), digits = 4),
                                  max = format(log10(apply(int_info, 2, max,na.rm=TRUE)), digits = 4),
                                  n = apply(int_info, 2, length)-colSums(is.na(int_info))))

    rownames(dynamic_table) <- gsub('Intensity', 'Log10 Intensity', rownames(dynamic_table))
  }


  dynamic_table$Experiment  <- rownames(dynamic_table)

  rownames(dynamic_table) <- NULL

  dynamic_table <- dynamic_table[,c(7,1,2,3,4,5,6)]
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



  #Table 4, GRAVY with median and retention times
  peptides <- files[['peptides.txt']]

  df <- peptides %>%  select(contains(c('Length',"Count","Sequence","Experiment")))



  df$GRAVY <-  (df$`A Count` * 1.8 +
                  df$`R Count` * -4.5 +
                  df$`N Count` * -3.5 +
                  df$`D Count` * -3.5 +
                  df$`C Count` * 2.5 +
                  df$`Q Count` * -3.5 +
                  df$`E Count` * -3.5 +
                  df$`G Count` * -0.4 +
                  df$`H Count` * -3.2 +
                  df$`I Count` * 4.5 +
                  df$`L Count` * 3.8 +
                  df$`K Count` * -3.9 +
                  df$`M Count` * 1.9 +
                  df$`F Count` * 2.8 +
                  df$`P Count` * -1.6 +
                  df$`S Count` * -0.8 +
                  df$`T Count` * -0.7 +
                  df$`W Count` * -0.9 +
                  df$`Y Count` * -1.3 +
                  df$`V Count` * 4.2)/df$Length


  df <- df %>% select(contains(c('GRAVY', 'Experiment')))

  df_out <- melt(df, id.vars = 'GRAVY')

  df_out$variable <- gsub('Experiment ', '', df_out$variable)

  #Remove value 0,

  #df_out<-df_out[!is.na(df_out$value),]

  df_out[is.na(df_out$value),] <- 0

  #Repeat rows n numbers of times, being n the frequency (value)
  df_expanded<- df_out[rep(rownames(df_out),df_out$value),]

  GRAVY <- df_expanded %>%
                group_by(variable) %>%
                summarise(Mean = mean(GRAVY),
                          Max = max(GRAVY),
                          Min = min(GRAVY),
                          Median = median(GRAVY),
                          Std = sd(GRAVY))
  names(GRAVY)[1] <- 'Experiment'

  GRAVY

   out <- list()

  out$proteins <- table_proteins
  out$intensities <- dynamic_table
  out$charge <- charge_percentage
  out$GRAVY <- GRAVY




  return(out)


}

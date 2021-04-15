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
#' data("MQmetrics_example_data")
#' ReportTables(MQPathCombined)
#'
ReportTables <- function(MQPathCombined,
                         log_base = 2,
                         intensity_type = 'Intensity'){

  sd <- median <- Experiment <- Charge <- variable <- NULL

  files <- MQmetrics::ReadDataFromDir(MQPathCombined)

    #Read the Protein Groups without removing the contamintants To plot it.
  proteinGroups <- read_delim(file.path(MQPathCombined,"txt/proteinGroups.txt"),
                              "\t", escape_double = FALSE,
                              trim_ws = TRUE)

  if (intensity_type == 'Intensity') {


    protein_table <- proteinGroups %>%
      select(contains(c('Intensity ','Reverse','Potential','Only identified by site'))) %>%
      select(-contains('LFQ'))
    #Remove Intensity from name
    colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))

    title <- 'Proteins Identified based on Intensity'

  }

  if (intensity_type == 'LFQ'){

    #Remove LFQ Intensity from name

    protein_table <- proteinGroups %>%
      select(contains(c('LFQ ','Reverse','Potential','Only identified by site')))


    colnames(protein_table) <- gsub("LFQ intensity.", "", colnames(protein_table))
    title <- 'Proteins Identified based on LFQ intensity'

    #Error if LFQ Intensity not found.

    if (length(protein_table) == 0) {
      print('LFQ intensities not found, changing automatically to Intensity.')

      protein_table <- proteinGroups %>%
        select(contains(c('Intensity ','Reverse','Potential','Only identified by site'))) %>%
        select(-contains('LFQ'))
      #Remove Intensity from name
      colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))

      title <- 'Proteins Identified based on Intensity'

    }

  }


  table_summary <- data.frame("Proteins Identified" = nrow(protein_table)-colSums(protein_table==0),
                              "Missing values" = colSums(protein_table==0),
                              "Potential contaminants" = colSums(protein_table[grep('+', protein_table$`Potential contaminant`),] >0),
                              "Reverse" = colSums(protein_table[grep('+', protein_table$Reverse),] >0),
                              "Only identified by site"= colSums(protein_table[grep('+', protein_table$`Only identified by site`),] >0),
                              check.names = FALSE) #To have spaces in the column names.

  table_summary <- table_summary[!(row.names(table_summary) %in% c("Reverse","Potential contaminant", "Only identified by site")),]

  table_summary$Experiment <- rownames(table_summary)

  rownames(table_summary) <- NULL

  table_summary <- table_summary[,c(6,1,2,3,4,5)]


  #Table 2 Log10 intensities


  int_info <- protein_table %>% select(-contains(c('Reverse', 'Potential contaminant','Only identified by site' )))

  int_info[int_info == 0] <- NA

  #log intensities

  if(log_base == 2){
    dynamic_table <- do.call(data.frame,
                             list(mean = format(log2(apply(int_info, 2, mean,na.rm=TRUE)), digits = 4),
                                  sd = format(log2(apply(int_info, 2, sd,na.rm=TRUE)), digits = 4),
                                  median = format(log2(apply(int_info, 2, median,na.rm=TRUE)), digits =4),
                                  min = format(log2(apply(int_info, 2, min,na.rm=TRUE)), digits = 4),
                                  max = format(log2(apply(int_info, 2, max,na.rm=TRUE)), digits = 4),
                                  n = apply(int_info, 2, length)-colSums(is.na(int_info))))
  }

  if(log_base == 10){
    dynamic_table <- do.call(data.frame,
                             list(mean = format(log10(apply(int_info, 2, mean,na.rm=TRUE)), digits = 4),
                                  sd = format(log10(apply(int_info, 2, sd,na.rm=TRUE)), digits = 4),
                                  median = format(log10(apply(int_info, 2, median,na.rm=TRUE)), digits =4),
                                  min = format(log10(apply(int_info, 2, min,na.rm=TRUE)), digits = 4),
                                  max = format(log10(apply(int_info, 2, max,na.rm=TRUE)), digits = 4),
                                  n = apply(int_info, 2, length)-colSums(is.na(int_info))))
  }


  dynamic_table$Experiment  <- rownames(dynamic_table)

  rownames(dynamic_table) <- NULL

  dynamic_table <- dynamic_table[,c(7,1,2,3,4,5,6)]

  #Table 3 Charge

  evidence <- files[["evidence.txt"]]

  charge_table <- evidence %>%
                      select(c(Experiment,Charge ))

  charge_table <- dcast(charge_table, Experiment~ Charge, fill = 0)

  charge_percentage <- charge_table[,-1]/rowSums(charge_table[,-1])*100

  charge_percentage <- cbind(charge_table$Experiment, format(round(charge_percentage,1), nsmall =1))

  names(charge_percentage)[1] <- 'Experiment'



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


  df_out[is.na(df_out$value),] <- 0

  #Repeat rows n numbers of times, being n the frequency (value)
  df_expanded<- df_out[rep(rownames(df_out),df_out$value),]

  GRAVY <- df_expanded %>%
                group_by(variable) %>%
                summarise(Mean = format(round(mean(GRAVY),2),nsmall = 1),
                          Max = format(round(max(GRAVY),2),nsmall = 1),
                          Min = format(round(min(GRAVY),2),nsmall = 1),
                          Median = format(round(median(GRAVY),2),nsmall = 1))
  names(GRAVY)[1] <- 'Experiment'



  # Table 5

  peptides <- files[["peptides.txt"]] %>%  select(contains(c('Missed cleavages', 'Experiment', 'Length')))

  pep_melt <-  melt(peptides, id.vars =c("Missed cleavages", 'Length'), measure.vars = colnames(peptides %>% select(contains(c('Experiment')))))
  pep_melt <- aggregate(value ~ variable + `Missed cleavages`, data=pep_melt, sum)


  missed_summary <- pep_melt %>%
    group_by(variable, `Missed cleavages`) %>%
    summarise(freq = sum(value))

  missed_summary <- pivot_wider(missed_summary, names_from =  `Missed cleavages`, values_from = freq)

  missed_summary$variable <- gsub('Experiment', '', missed_summary$variable)
  colnames(missed_summary)[colnames(missed_summary)=='variable'] <- 'Experiment'




  out <- list()

  out$proteins <- table_summary
  out$intensities <- dynamic_table
  out$charge <- charge_percentage
  out$GRAVY <- GRAVY
  out$cleavages <- missed_summary

  return(out)
}

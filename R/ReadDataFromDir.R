#' Read MaxQuant Tables From Directory
#'
#' @param x The directory containing the MaxQuant Output.
#' @param remove_contaminants Whether or not to remove contaminants, reverse and identified by one one peptide.
#'
#' @return The output from \code{ReadDataFromR}
#' @export
#'
#'@import ggplot2
#'@importFrom readr read_delim
#'@importFrom magrittr %>%
#'@importFrom dplyr contains
#'@importFrom reshape2 melt dcast
#'@importFrom dplyr select group_by filter
#'@importFrom gridExtra marrangeGrob
#'@importFrom utils head
#'@importFrom stringr str_count
#'@importFrom chron times
#'
#'
#' @examples
ReadDataFromDir <- function(x, remove_contaminants = TRUE){

  #open summary.txt
  summary_table <- read_delim(file.path(x, "txt/summary.txt"),
                              "\t", escape_double = FALSE, trim_ws = TRUE,na = c("NA", "NaN", "", " "))

  summary_table <- head(summary_table,-1)


  #Open the peptides.txt table

  peptides_table <- read_delim(file.path(x,"txt/peptides.txt"),
                               "\t", escape_double = FALSE,na = c("NA", "NaN", "", " "),trim_ws = TRUE)

  if(remove_contaminants==TRUE){

    peptides_table <- peptides_table[is.na(peptides_table$`Potential contaminant`) & is.na(peptides_table$Reverse),]
  }

  #Open the evidence table
  evidence_table <- read_delim(file.path(x,"txt/evidence.txt"),
                               "\t", escape_double = FALSE,na = c("NA", "NaN", "", " "),trim_ws = TRUE)

  if(remove_contaminants==TRUE){

  evidence_table <- evidence_table[is.na(evidence_table$`Potential contaminant`) & is.na(evidence_table$Reverse),]
  }




  #msmsScans.txt

  msscans_table <- read_delim(file.path(x,"txt/msmsScans.txt"),
                              "\t", escape_double = FALSE, na = c("NA", "NaN", "", " "),
                              trim_ws = TRUE)

  if(remove_contaminants==TRUE){
  msscans_table <- msscans_table[is.na(msscans_table$Reverse),]
  }
  #proteinGroups.txt

  prot_groups <- read_delim(file.path(x,"txt/proteinGroups.txt"),
                            "\t", escape_double = FALSE,
                            trim_ws = TRUE)

  if(remove_contaminants==TRUE){
  prot_groups <- prot_groups[is.na(prot_groups$`Potential contaminant`) & is.na(prot_groups$Reverse)  & is.na(prot_groups$`Only identified by site`),]

  }




  #runningTimes.txt

  running_time <- read_delim(file.path(x, "#runningTimes.txt"),
                             "\t", escape_double = FALSE, trim_ws = TRUE,na = c("NA", "NaN", "", " "))


  #parameters.txt
  parameters_table <- read_delim(file.path(x,"txt/parameters.txt"),
                           "\t", escape_double = FALSE, na = "NA",
                           trim_ws = TRUE)


  #Vector of tables

  alltables <- list(summary_table, peptides_table, evidence_table,  msscans_table, prot_groups, running_time, parameters_table)

  names(alltables) <- c('summary.txt', 'peptides.txt', 'evidence.txt','msmsScans.txt', 'proteinGroups.txt', 'runningTimes.txt', 'parameters.txt')


  return(alltables)

}

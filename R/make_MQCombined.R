#' Read MaxQuant Tables From Directory
#'
#' @param MQPathCombined The directory to the "combined" folder where the
#' MaxQuant results are stored.
#' @param remove_contaminants Whether or not to remove contaminants,
#' reverse and identified by one one peptide.
#'
#' @return The files from the MaxQuant with the contaminants and Reverse
#' hits removed.
#' @export
#'
#'
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- make_MQCombined(MQPathCombined)
make_MQCombined <- function(MQPathCombined,
                            remove_contaminants = TRUE){

  #open summary.txt
  summary_table <- read_delim(file.path(MQPathCombined, "txt/summary.txt"),
                              "\t",
                              escape_double = FALSE,
                              trim_ws = TRUE,
                              na = c("NA", "NaN", "", " "))

  summary_table <- head(summary_table,-1)


  #Open the peptides.txt table

  peptides_table <- read_delim(file.path(MQPathCombined,"txt/peptides.txt"),
                               "\t",
                               escape_double = FALSE,
                               na = c("NA", "NaN", "", " "),
                               trim_ws = TRUE,
                               guess_max = 10**6)

  if(remove_contaminants==TRUE){

    peptides_table <- peptides_table[
        is.na(peptides_table$`Potential contaminant`)
        & is.na(peptides_table$Reverse),]
  }

  #Open the evidence table
  evidence_table <- read_delim(file.path(MQPathCombined,"txt/evidence.txt"),
                               "\t",
                               escape_double = FALSE,
                               na = c("NA", "NaN", "", " "),
                               trim_ws = TRUE,
                               guess_max = 10**6)

  if(remove_contaminants==TRUE){

    evidence_table <- evidence_table[
        is.na(evidence_table$`Potential contaminant`)
        & is.na(evidence_table$Reverse),]
  }


  #msScans.txt
  msScans_table <- read_delim(file.path(MQPathCombined,"txt/msScans.txt"),
                              "\t",
                              escape_double = FALSE,
                              na = c("NA", "NaN", "", " "),
                              trim_ws = TRUE)


  #msmsScans.txt

  msscans_table <- read_delim(file.path(MQPathCombined,"txt/msmsScans.txt"),
                              "\t",
                              escape_double = FALSE,
                              na = c("NA", "NaN", "", " "),
                              trim_ws = TRUE,
                              guess_max = 10**6)

  if(remove_contaminants==TRUE){
    msscans_table <- msscans_table[is.na(msscans_table$Reverse),]
  }
  #proteinGroups.txt

  prot_groups <- read_delim(file.path(MQPathCombined,"txt/proteinGroups.txt"),
                            "\t", escape_double = FALSE,
                            trim_ws = TRUE, guess_max = 10**5)

  if(remove_contaminants==TRUE){
    prot_groups <- prot_groups[
        is.na(prot_groups$`Potential contaminant`)
        & is.na(prot_groups$Reverse)
        & is.na(prot_groups$`Only identified by site`),]

  }


  #modificationSpecificPeptides

  modification_table <-
      read_delim(file.path(MQPathCombined,
                           "txt/modificationSpecificPeptides.txt"),
                                   "\t",
                                   escape_double = FALSE,
                                   na = c("NA", "NaN", "", " "),
                                   trim_ws = TRUE,
                                   guess_max = 10**6)
  if(remove_contaminants==TRUE){

    modification_table <- modification_table[
        is.na(modification_table$`Potential contaminant`)
        & is.na(modification_table$Reverse),]
  }

  #parameters.txt
  parameters_table <- read_delim(file.path(MQPathCombined,"txt/parameters.txt"),
                                 "\t",
                                 escape_double = FALSE,
                                 na = "NA",
                                 trim_ws = TRUE)


  #Check if the running times is present, if so, add it to alltables
  if ('#runningTimes.txt' %in% list.files(file.path(MQPathCombined, 'proc/'))) {



    running_time <- read_delim(file.path(MQPathCombined,
                                         "proc/#runningTimes.txt"),
                               "\t",
                               escape_double = FALSE,
                               trim_ws = TRUE,
                               na = c("NA", "NaN", "", " "))


    alltables <- list(MQPathCombined,summary_table, peptides_table, evidence_table,
                      msscans_table, msScans_table,prot_groups,
                      modification_table, parameters_table, running_time)

    names(alltables) <- c('MQPathCombined','summary.txt', 'peptides.txt', 'evidence.txt',
                          'msmsScans.txt', 'msScans.txt','proteinGroups.txt',
                          'modificationSpecificPeptides.txt', 'parameters.txt',
                          '#runningTimes.txt')

  } else{
    print('#runningTimes.txt not found.')

    alltables <- list(MQPathCombined,summary_table, peptides_table, evidence_table,
                      msscans_table, msScans_table,prot_groups,
                      modification_table, parameters_table)

    names(alltables) <- c('MQPathCombined','summary.txt', 'peptides.txt', 'evidence.txt',
                          'msmsScans.txt', 'msScans.txt','proteinGroups.txt',
                          'modificationSpecificPeptides.txt', 'parameters.txt')

  }

  return(alltables)
}

#' Experiment Information
#'
#' @param running_times The runningTimes.txt table from  MaxQuant Output.
#' @param  parameters The parameters.txt table from  MaxQuant Output.
#' @return Returns the time in hours:minutes that lasted the whole Experiment.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' runningTimes <-  files[["#runningTimes.txt"]]
#' parameters <- files[["parameters.txt"]]
#' ExperimentInformation(runningTimes, parameters)
#'
ExperimentInformation <- function(runningTimes,
                                  parameters){
  #Runningtimes
  time <- sum(runningTimes$`Running time [min]`)

  time <- substr(times((time%/%60 +  time%%60 /60)/24), 1, 5)

  start_date <- runningTimes$`Start date`[1]

  start_time <- runningTimes$`Start time`[1]

  #Parameters

  MaxQuant_version <- parameters$Value[1]

  user_name <-  parameters$Value[2]

  machine_name <- parameters$Value[3]

  Protein_FDR <- parameters$Value[8]

  match_between_runs <- parameters$Value[27]

  fasta_file <- parameters$Value[33]

  print(paste0('The experiment started the day: ', start_date, ' at the time: ', start_time, '.'))
  print(paste0('The whole experiment lasted: ', time, ' (hours:minutes).'))
  print(paste0('The MaxQuant version used was: ', MaxQuant_version))
  print(paste0('The user was: ', user_name))
  print(paste0('The machine name was: ', machine_name))
  print(paste0('The protein FDR was: ', Protein_FDR))
  print(paste0('The match between runs was: ', match_between_runs))
  print(paste0('The fasta file used was: ', fasta_file))
}

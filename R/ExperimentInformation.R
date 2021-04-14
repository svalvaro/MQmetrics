#' Experiment Information
#' @param MQPathCombined The directory to the "combined" folder where the
#'  MaxQuant results are stored.
#' @param runningTimes The runningTimes.txt table from  MaxQuant Output.
#' @param  parameters The parameters.txt table from  MaxQuant Output.
#' @return Returns the time in hours:minutes that lasted the whole Experiment.
#' @export
#'
#' @examples
#' data("MQmetrics_example_data")
#' ExperimentInformation(MQPathCombined, runningTimes, parameters)
#'
ExperimentInformation <- function(MQPathCombined,
                                  runningTimes,
                                  parameters){
  #Runningtimes
  time <- sum(runningTimes$`Running time [min]`)

  time <- substr(times((time%/%60 +  time%%60 /60)/24), 1, 5)

  start_date <- runningTimes$`Start date`[1]

  start_time <- runningTimes$`Start time`[1]

  #Parameters

  MaxQuant_version <- parameters$Value[parameters$Parameter == 'Version']

  user_name <-  parameters$Value[parameters$Parameter == 'User name']

  machine_name <- parameters$Value[parameters$Parameter == 'Machine name']

  PSM_FDR <- parameters$Value[parameters$Parameter == 'PSM FDR']

  Protein_FDR <- parameters$Value[parameters$Parameter == 'Protein FDR']

  match_between_runs <- parameters$Value[parameters$Parameter == 'Match between runs']

  fasta_file <- parameters$Value[parameters$Parameter == 'Fasta file']

  iBAQ <- parameters$Value[parameters$Parameter == 'iBAQ']

  PTM <- parameters$Value[parameters$Parameter == 'Modifications included in protein quantification']

  print(paste0('The MaxQuant output directory is: ', MQPathCombined))
  print(paste0('The experiment started the day: ', start_date, ' at the time: ', start_time, '.'))
  print(paste0('The whole experiment lasted: ', time, ' (hours:minutes).'))
  print(paste0('The MaxQuant version used was: ', MaxQuant_version))
  print(paste0('The user was: ', user_name))
  print(paste0('The machine name was: ', machine_name))
  print(paste0('The PSM FDR was: ', PSM_FDR))
  print(paste0('The protein FDR was: ', Protein_FDR))
  print(paste0('The match between runs was: ', match_between_runs))
  print(paste0('The fasta file used was: ', fasta_file))
  print(paste0('The iBAQ presence is: ', iBAQ))
  print(paste0('The PTM selected is/are: ', PTM))
}

#' Experiment Information
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @return Returns the time in hours:minutes that lasted the whole Experiment.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' MaxQuantAnalysisInfo(MQCombined)
#'
MaxQuantAnalysisInfo <- function(MQCombined){
    MQPathCombined <- MQCombined$MQPathCombined
    runningTimes <- MQCombined$`#runningTimes.txt`
    parameters <- MQCombined$parameters.txt
    #Runningtimes
    time <- sum(runningTimes$`Running time [min]`)

    days <- time%/%1440

    hours <- (time%%1440)%/%60

    mins <- round((time%%1440)%%60)

    start_date <- runningTimes$`Start date`[1]

    start_time <- runningTimes$`Start time`[1]

    end_date <- runningTimes$`End date`[nrow(runningTimes)]

    end_time <- runningTimes$`End time`[nrow(runningTimes)]


    #Parameters

    MaxQuant_version <- parameters$Value[parameters$Parameter == 'Version']

    user_name <-  parameters$Value[parameters$Parameter == 'User name']

    machine_name <- parameters$Value[parameters$Parameter == 'Machine name']

    PSM_FDR <- parameters$Value[parameters$Parameter == 'PSM FDR']

    Protein_FDR <- parameters$Value[parameters$Parameter == 'Protein FDR']

    match_between_runs <- parameters$Value[
        parameters$Parameter == 'Match between runs']

    fasta_file <- parameters$Value[parameters$Parameter == 'Fasta file']

    iBAQ <- parameters$Value[parameters$Parameter == 'iBAQ']

    PTM <- parameters$Value[
        parameters$Parameter ==
            'Modifications included in protein quantification'
        ]

    print(paste0('The MaxQuant output directory is: ', MQPathCombined))
    print(paste0('The experiment started the day: ',
                start_date, ' at the time: ',
                start_time, '.'))
    print(paste0('The whole experiment lasted: ',days ,' days, ',
                 hours,' hours and ', mins, ' mins.'))
    print(paste0('The Experiment finished on: ', end_date, ' at: ', end_time))
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

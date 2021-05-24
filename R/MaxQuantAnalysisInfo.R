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

    fasta_file <- gsub(';', '\n', fasta_file)

    iBAQ <- parameters$Value[parameters$Parameter == 'iBAQ']

    PTM <- parameters$Value[
        parameters$Parameter ==
            'Modifications included in protein quantification'
        ]

    cat(paste0('The MaxQuant output directory is: \n',
                 MQPathCombined))
    cat(paste0('\nThe experiment started the day: ',
                start_date, ' at the time: ',
                start_time, '.'))
    cat(paste0('\nThe whole experiment lasted: ',days ,' days, ',
                 hours,' hours and ', mins, ' mins.'))
    cat(paste0('\nThe Experiment finished on: ', end_date, ' at: ', end_time))
    cat(paste0('\nThe MaxQuant version used was: ', MaxQuant_version))
    cat(paste0('\nThe user was: ', user_name))
    cat(paste0('\nThe machine name was: ', machine_name))
    cat(paste0('\nThe PSM FDR was: ', PSM_FDR))
    cat(paste0('\nThe protein FDR was: ', Protein_FDR))
    cat(paste0('\nThe match between runs was: ', match_between_runs))
    cat(paste0('\nThe fasta file(s) used was: \n', fasta_file))
    cat(paste0('\nThe iBAQ presence is: ', iBAQ))
    cat(paste0('\nThe PTM selected is/are: ', PTM))
}

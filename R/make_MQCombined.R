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
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
make_MQCombined <- function(MQPathCombined,
                            remove_contaminants = TRUE) {


    allTables <- list()


    # open summary.txt

    if ("summary.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        summary_table <- read_delim(file.path(
            MQPathCombined,
            "txt/summary.txt"),
            "\t",
            escape_double = FALSE,
            trim_ws = TRUE,
            na = c("NA", "NaN", "", " ")
        )

        summary_table <- head(summary_table, -1)

        allTables[[length(allTables)+1]] <- summary_table
        #names(allTables[]) <- 'summary.txt'
        names(allTables)[[length(allTables)]] <- 'summary.txt'
    }else{
        warning('summary.txt not found, some plots will be missing')
        print('summary.txt not found, some plots will be missing')
    }




    # Open the peptides.txt table


    if ("peptides.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        peptides_table <- read_delim(
            file.path(
                MQPathCombined,
                "txt/peptides.txt"),
            "\t",
            escape_double = FALSE,
            na = c("NA", "NaN", "", " "),
            trim_ws = TRUE,
            guess_max = 10**6
        )

        if (remove_contaminants == TRUE) {
            peptides_table <- peptides_table[
                is.na(peptides_table$`Potential contaminant`) &
                    is.na(peptides_table$Reverse),]
        }

        allTables[[length(allTables)+1]] <- peptides_table
        names(allTables)[[length(allTables)]] <- 'peptides.txt'

    }else{
        warning('peptides.txt not found, some plots will be missing')
        print('peptides.txt not found, some plots will be missing')
    }


    # Open the evidence table

    if ("evidence.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        evidence_table <- read_delim(
            file.path(
                MQPathCombined,
                "txt/evidence.txt"),
            "\t",
            escape_double = FALSE,
            na = c("NA", "NaN", "", " "),
            trim_ws = TRUE,
            guess_max = 10**6
        )

        if (remove_contaminants == TRUE) {
            evidence_table <- evidence_table[
                is.na(evidence_table$`Potential contaminant`) &
                    is.na(evidence_table$Reverse),
            ]
        }

        allTables[[length(allTables)+1]] <- evidence_table
        names(allTables)[[length(allTables)]] <- 'evidence.txt'

    }else{
        warning('evidence.txt not found, some plots will be missing')
        print('evidence.txt not found, some plots will be missing')
    }


    # Check if the msScans.txt is present, if so, add it to alltables

    if ("msScans.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        msScans_table <- read_delim(
            file.path(
                MQPathCombined, "txt/msScans.txt"),
            "\t",
            escape_double = FALSE,
            na = c("NA", "NaN", "", " "),
            trim_ws = TRUE
        )


        allTables[[length(allTables)+1]] <- msScans_table
        names(allTables)[[length(allTables)]] <- 'msScans.txt'

    }else{

        warning('msScans.txt not found, some plots will be missing')
        print('msScans.txt not found, some plots will be missing')
    }


    # msmsScans.txt

    if ("msmsScans.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        msscans_table <- read_delim(
            file.path(
                MQPathCombined, "txt/msmsScans.txt"),
            "\t",
            escape_double = FALSE,
            na = c("NA", "NaN", "", " "),
            trim_ws = TRUE,
            guess_max = 10**6
        )

        if (remove_contaminants == TRUE) {
            msscans_table <- msscans_table[is.na(msscans_table$Reverse), ]
        }


    allTables[[length(allTables)+1]] <- msscans_table
    names(allTables)[[length(allTables)]] <- 'msmsScans.txt'

    }else{
        warning('msmsScans.txt not found, some plots will be missing')
        print('msmsScans.txt not found, some plots will be missing')
    }


    # proteinGroups.txt

    if ("proteinGroups.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        prot_groups <- read_delim(
            file.path(
                MQPathCombined,
                "txt/proteinGroups.txt"
            ),
            "\t",
            escape_double = FALSE,
            trim_ws = TRUE,
            guess_max = 10**5
        )

        if (remove_contaminants == TRUE) {
            prot_groups <- prot_groups[
                is.na(prot_groups$`Potential contaminant`) &
                    is.na(prot_groups$Reverse) &
                    is.na(prot_groups$`Only identified by site`),
            ]
        }


        allTables[[length(allTables)+1]] <- prot_groups
        names(allTables)[[length(allTables)]] <- 'proteinGroups.txt'

    }else{
        warning('proteinGroups.txt not found, some plots will be missing')
        print('proteinGroups.txt not found, some plots will be missing')
    }



    # modificationSpecificPeptides

    if ("modificationSpecificPeptides.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        modification_table <-
            read_delim(
                file.path(
                    MQPathCombined,
                    "txt/modificationSpecificPeptides.txt"
                ),
                "\t",
                escape_double = FALSE,
                na = c("NA", "NaN", "", " "),
                trim_ws = TRUE,
                guess_max = 10**6
            )
        if (remove_contaminants == TRUE) {
            modification_table <- modification_table[
                is.na(modification_table$`Potential contaminant`) &
                    is.na(modification_table$Reverse),
            ]
        }

        allTables[[length(allTables)+1]] <- modification_table
        names(allTables)[[length(allTables)]] <- 'modificationSpecificPeptides.txt'

    }else{
        warning('modificationSpecificPeptides.txt not found, some plots will be missing')
        print('modificationSpecificPeptides.txt not found, some plots will be missing')
    }


    # parameters.txt

    if ("parameters.txt" %in% list.files(file.path(MQPathCombined,"txt/"))){

        # parameters.txt
        parameters_table <- read_delim(file.path(
            MQPathCombined,
            "txt/parameters.txt"
        ),
        "\t",
        escape_double = FALSE,
        na = "NA",
        trim_ws = TRUE
        )

        allTables[[length(allTables)+1]] <- parameters_table
        names(allTables)[[length(allTables)]] <- 'parameters.txt'

    }else{
        warning('parameters.txt not found, some plots will be missing')
        print('parameters.txt not found, some plots will be missing')
    }



    # running Times
    if ("#runningTimes.txt" %in% list.files(file.path(MQPathCombined,"proc/"))){


        running_time <- read_delim(file.path(
            MQPathCombined,
            "proc/#runningTimes.txt"
        ),
        "\t",
        escape_double = FALSE,
        trim_ws = TRUE,
        na = c("NA", "NaN", "", " ")
        )

        allTables[[length(allTables)+1]] <- running_time
        names(allTables)[[length(allTables)]] <- '#runningTimes.txt'

    }else{
        warning('#runningTimes.txt not found, some plots will be missing')
        print('#runningTimes.txt not found, some plots will be missing')
    }


    # Add MQPath Combined

    allTables[[length(allTables)+1]] <- MQPathCombined
    names(allTables)[[length(allTables)]] <- 'MQPathCombined'


    return(allTables)
}

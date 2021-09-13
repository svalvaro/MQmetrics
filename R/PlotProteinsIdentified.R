#' Proteins Identified per sample.
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param intensity_type The type of intensity. Values: 'Intensity' or 'LFQ'.
#'  Only useful if split_violin_intensity = FALSE.  Default is Intensity.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#'
#' @return A plot showing the number of proteins identified per sample and the
#'  number of missing values.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotProteinsIdentified(MQCombined)
PlotProteinsIdentified <- function(MQCombined,
                                intensity_type = "Intensity",
                                long_names = FALSE,
                                sep_names = NULL,
                                palette = "Set2") {

    proteinGroups <- MQCombined$proteinGroups.txt

    Experiment <- value <- variable <- NULL

    if (intensity_type == "Intensity") {

        protein_table <- proteinGroups %>%
        select(contains(
            c('Intensity ', 'Identification type'))) %>%
        select(-contains('LFQ'))

        #protein_table <- proteinGroups[, grep("Intensity ",
                                              #colnames(proteinGroups))]
        # Remove Intensity from name
        colnames(protein_table) <- gsub("Intensity.", "",
                                        colnames(protein_table))

        #title <- "Protein Identification based on Intensity"
        title <- "Protein Identification"
    }

    if (intensity_type == "LFQ") {

        protein_table <- proteinGroups %>%
            select(contains(
                c('LFQ intensity', 'Identification type')))

        #protein_table <- proteinGroups[, grep("LFQ", colnames(proteinGroups))]

        # Remove LFQ Intensity from name
        colnames(protein_table) <- gsub("LFQ intensity.","",
            colnames(protein_table))

        title <- "Protein Identification based on LFQ intensity"

        # Error if LFQ Intensity not found.

        if (length(protein_table) == 0) {
            print("LFQ intensities not found,
                changing automatically to Intensity.")

            protein_table <- proteinGroups %>%
                select(contains(
                    c('Intensity ', 'Identification type'))) %>%
                select(-contains('LFQ'))

            #protein_table <- proteinGroups[, grep("Intensity ",
            #colnames(proteinGroups))]
            # Remove Intensity from name
            colnames(protein_table) <- gsub("Intensity.", "",
                                            colnames(protein_table))

            #title <- "Protein Identification based on Intensity"
            title <- "Protein Identification"
        }
    }

    # Count the missing values
    missing_values <- protein_table %>%
        select(-contains('Identification type'))

    missing_values <- data.frame(
        `Missing values` = nrow(missing_values) - colSums( missing_values> 0,)
        )


    # Add exception if MBR is false

    MBR <- MQCombined$parameters$Value[
        parameters$Parameter == 'Match between runs']

    if (MBR == 'True') {

        # Count the identification type
        identification_type <- protein_table %>%
            select(contains('Identification type'))

        df <- data.frame(
            `By Matching` = str_count(identification_type, 'By matching'),
            `By MS/MS` = str_count(identification_type, 'By MS/MS'))

        # Bind them together
        df <- cbind(missing_values,df)



        # If MBR is false
    } else{

        df <- missing_values

        df$Identified <- nrow(protein_table) - df$Missing.values


    }
    #
    df$Experiment <- rownames(df)

    rownames(df) <- NULL

    df <- melt(df, id.vars = 'Experiment')


    a <-    ggplot(df, aes(x = Experiment, y = value, fill = variable))+
                ggtitle(title) +
                geom_bar(stat = "identity",
                         position = "stack",
                         size = 0.5,
                         col = "black") +
                theme(axis.title.y = element_text(margin = margin(r = 20))) +
                ylab("Number of Proteins") +
                theme_bw() +
                scale_fill_brewer(palette = palette) +
                #scale_fill_manual(values = c('olivedrab3','pink4')) +
                theme(legend.position = "bottom",
                      legend.title = element_blank())

    if (long_names == TRUE) {
        a + scale_x_discrete(labels = function(x) {
            stringr::str_wrap(gsub(sep_names," ", x),3)})
    } else {
        a
    }
}

#' Comparison of the MS/MS submitted and identified in each sample.
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param position_dodge_width Position of the columns within each others.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#'
#' @return Plots the MS/MS submitted and Identified in each sample.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotMsMs(MQCombined)
PlotMsMs <- function(MQCombined,
                    long_names = FALSE,
                    sep_names = NULL,
                    position_dodge_width = 1,
                    palette = "Set2") {
    summary <- MQCombined$summary.txt

    Experiment <- `MS/MS Submitted` <- `MS/MS Identified` <- NULL
    value <- variable <- `MS/MS submitted` <- `MS/MS identified` <- NULL


    MaxQuant_version <- MQCombined$parameters$Value[
        MQCombined$parameters$Parameter == 'Version']

    #Detect MaxQuant Version to read column names accordingly.

    if (base::startsWith(MaxQuant_version, '1')) {

        a <- summary %>% select(c(Experiment, `MS/MS Submitted`,
                                  `MS/MS Identified`))

    } else{
        a <- summary %>% select(c(Experiment, `MS/MS submitted`,
                                  `MS/MS identified`))

    }




    a_melt <- melt(a, id.vars = "Experiment")

    b <- ggplot(a_melt, aes(
                            x = Experiment,
                            y = value,
                            group = variable,
                            fill = variable
    )) +
        geom_bar(
                stat = "identity",
                colour = "black",
                position = position_dodge(width = position_dodge_width)
        ) +
        theme_bw() +
        ylab("MS/MS Frequency") +
        ggtitle("MS/MS Submitted and Identified") +
        scale_fill_brewer(palette = palette) +
        theme(legend.position = "bottom")+
        labs(fill = element_blank())

    if (long_names == TRUE) {
        b + scale_x_discrete(labels = function(x) {
                stringr::str_wrap(gsub(sep_names," ",x),3)})
    } else {
        b
    }
}

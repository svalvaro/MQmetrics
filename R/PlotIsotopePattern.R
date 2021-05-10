#' Plot Isotope Pattern and Isotope Pattern Sequenced
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
#' @return Returns a plot Isotope Pattern and Isotope Pattern Sequenced.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotIsotopePattern(MQCombined)
PlotIsotopePattern <- function(MQCombined,
                               long_names = FALSE,
                               sep_names = NULL,
                               position_dodge_width = 1,
                               palette = "Set2") {
  summary <- MQCombined$summary.txt

  Experiment <- `Isotope Patterns` <- `Isotope Patterns Sequenced` <- NULL
  value <- variable <- NULL

  a <- summary %>% select(c(
    Experiment,
    `Isotope Patterns`,
    `Isotope Patterns Sequenced`
  ))

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
    ylab("Frequency Isotope Patterns") +
    ggtitle("Isotope Patterns detected and sequenced") +
    scale_fill_brewer(palette = palette) +
    theme(legend.position = "bottom")

  if (long_names == TRUE) {
    b + scale_x_discrete(labels = function(x) {
      stringr::str_wrap(
        gsub(
          sep_names,
          " ",
          x
        ),
        3
      )
    })
  } else {
    b
  }
}

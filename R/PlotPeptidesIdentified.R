#' Total number of peaks detected and sequenced
#'
#' @param summary The peptides.txt table from  MaxQuant Output.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Plots the total number of unique peptide amino acid sequences
#' identified from the recorded tandem mass spectra.
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' summary <- files[['summary.txt']]
#' PlotPeptidesIdentified(summary)
#'
PLotPeptidesIdentified <- function(summary,
                                   long_names = FALSE,
                                   sep_names = NULL,
                                   palette = 'Set2'){

  `Peptide Sequences Identified` <- Experiment<- NULL

  b <- ggplot(summary, aes(x=Experiment , y = `Peptide Sequences Identified`,
                            fill = Experiment))+
          geom_bar(stat = 'identity', color='black')+
          theme_bw(base_size = 12)+
          ggtitle('Peptides Sequences Identified')+
          theme(legend.position = 'none')+
          scale_fill_brewer(palette = palette)

  if (long_names == TRUE) {
    b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))

  } else{
    b
  }

}

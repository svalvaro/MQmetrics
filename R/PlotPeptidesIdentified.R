
#' Total number of peaks detected and sequenced
#'
#' @param summary The peptides.txt table from  MaxQuant Output.
#'
#' @return Plots the total number of unique peptide amino acid sequences identified from the recorded tandem mass spectra.
#' @export
#'
#' @examples
PLotPeptidesIdentified <- function(summary, long_names = FALSE,sep_names = '-'){
  `Peptide Sequences Identified` <- Experiment<- NULL

  b <- ggplot(summary, aes(x=Experiment , y = `Peptide Sequences Identified`,
                            fill = Experiment))+
          geom_bar(stat = 'identity', color='black')+
          theme_bw(base_size = 12)+
          ggtitle('Peptides Sequences Identified')+
          theme(legend.position = 'none')
          #

  if (long_names == TRUE) {
    b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))

  } else{
    b
  }

}

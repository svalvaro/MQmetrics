
#' Total number of peaks detected and sequenced
#'
#' @param peptides The peptides.txt table from  MaxQuant Output.
#'
#' @return Plots the total number of unique peptide amino acid sequences identified from the recorded tandem mass spectra.
#' @export
#'
#' @examples
PLotPeptidesIdentified <- function(peptides){
  `Peptide Sequences Identified` <- Experiment<- NULL

  ggplot(peptides, aes(x=Experiment , y = `Peptide Sequences Identified`, fill = Experiment))+
  geom_bar(stat = 'identity', color='black')+
  theme_bw(base_size = 12)+
  ggtitle('Peptides Sequences Identified')+
  theme(legend.position = 'none')
  #
  # scale_y_discrete(labels = function(x) stringr::str_wrap(gsub('_',' ',x), width_horizontal_plots))
  }

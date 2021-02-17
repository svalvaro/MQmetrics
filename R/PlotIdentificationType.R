
#' Compares the type of identification of each sample
#'
#' @param peptides  The peptides.txt table from  MaxQuant Output.
#'
#' @return Plots the compares of the type of identification of each sample. It will not work if in MaxQuant the Match Between Run was not selected.
#' @export
#'
#' @examples
PlotIdentificationType <- function(peptides){

  value <- variable <- NULL

  ide_type <- peptides %>% select(contains('Identification type'))

  #NAs <- sapply(ide_type, function(x) sum(is.na(x)))
  By_MS_MS  <- str_count(ide_type, 'By MS/MS')
  By_matching <- str_count(ide_type, 'By matching')

  #ide_data <- data.frame(By_MS_MS, By_matching,NAs)
  ide_data <- data.frame( By_matching,By_MS_MS)
  rownames(ide_data) <- colnames(ide_type)
  ide_data$sample <- rownames(ide_data)
  ide_data_melted <- melt(ide_data)

  ggplot(ide_data_melted, aes(x=sample, y=value, fill=variable))+
    geom_col()+
    ggtitle('Peptide Identification type')+
    geom_bar(stat = 'identity',position='stack',size=0.5,col="black")+
    theme(axis.title.y = element_text(margin = margin(r = 20)))#+
    #scale_x_discrete(labels = function(x) stringr::str_wrap(gsub('_',' ',x), width_vertical_plots))

}



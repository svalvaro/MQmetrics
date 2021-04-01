#' Protein and Peptide Identification type.
#'
#' @param peptides  The peptides.txt table from  MaxQuant Output.
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Plots the compares of the protein and peptide  identification of each sample.
#'  It will only work if in MaxQuant the Match Between Run was selected.
#'
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotIdentificationType(peptides,proteinGroups)
#'
#' # If there are long names separated by underscore:
#' PlotIdentificationType(peptides,proteinGroups, long_names = TRUE, sep_names = '_')
#'
PlotIdentificationType <- function(peptides,
                                   proteinGroups,
                                   long_names = FALSE,
                                   sep_names = NULL,
                                   palette = 'Set2'){

  value <- variable <- NULL

  #Peptide Identification type
  ide_type <- peptides %>% select(contains('Identification type'))

  #NAs <- sapply(ide_type, function(x) sum(is.na(x)))
  By_MS_MS  <- str_count(ide_type, 'By MS/MS')
  By_matching <- str_count(ide_type, 'By matching')

  #ide_data <- data.frame(By_MS_MS, By_matching,NAs)
  ide_data <- data.frame( By_matching,By_MS_MS)
  rownames(ide_data) <- colnames(ide_type)
  ide_data$sample <- rownames(ide_data)
  ide_data_melted <- melt(ide_data)

  ide_data_melted$sample <- gsub("Identification type", "", paste(ide_data_melted$sample))

  a <- ggplot(ide_data_melted, aes(x=sample, y=value, fill=variable))+
          geom_col()+
          ggtitle('Peptide Identification type')+
          geom_bar(stat = 'identity',position='stack',size=0.5,col="black")+
          theme(axis.title.y = element_text(margin = margin(r = 20)))+
          theme_bw()+
          scale_fill_brewer(palette = palette)+
          theme(legend.position = 'bottom')

  if (long_names == TRUE) {
    a <- a + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))

  } else{
    a
  }


   #Protein Identification Type

  prot_ide_type <- proteinGroups %>% select(contains('Identification type'))

  #NAs <- sapply(ide_type, function(x) sum(is.na(x)))
  by_MS_MS  <- str_count(prot_ide_type, 'By MS/MS')
  by_matching <- str_count(prot_ide_type, 'By matching')

  #ide_data <- data.frame(By_MS_MS, By_matching,NAs)
  prot_data <- data.frame( by_matching,by_MS_MS)
  rownames(prot_data) <- colnames(prot_ide_type)
  prot_data$sample <- rownames(prot_data)
  prot_data_melted <- melt(prot_data)

  prot_data_melted$sample <- gsub("Identification type", "", paste(prot_data_melted$sample))

  b <- ggplot(prot_data_melted, aes(x=sample, y=value, fill=variable))+
    geom_col()+
    ggtitle('Protein Identification type')+
    geom_bar(stat = 'identity',position='stack',size=0.5,col="black")+
    theme(axis.title.y = element_text(margin = margin(r = 20)))+
    theme_bw()+
    scale_fill_brewer(palette = palette)+
    theme(legend.position = 'bottom')

  if (long_names == TRUE) {
    b <- b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))

  } else{
    b
  }

  plot_grid(
    b, a,
    ncol = 1
  )



}



#' Proteins Identified per sample.
#'
#' @param proteinGroups  The proteinGroups.txt table from  MaxQuant Output.
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
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotProteinsIdentified(proteinGroups)
#'
PlotProteinsIdentified <- function(MQCombined,
                                   intensity_type = 'Intensity',
                                   long_names = FALSE,
                                   sep_names = NULL,
                                   palette = 'Set2'){

    proteinGroups <- MQCombined$proteinGroups.txt

  Experiment <- value <- variable <- NULL

  if (intensity_type == 'Intensity') {
    protein_table <- proteinGroups[,grep("Intensity ", colnames(proteinGroups))]
    #Remove Intensity from name
    colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))

    title <- 'Proteins Identified based on Intensity'

  }

  if (intensity_type == 'LFQ'){
    protein_table<- proteinGroups[,grep("LFQ", colnames(proteinGroups))]
    #Remove LFQ Intensity from name
    colnames(protein_table) <- gsub("LFQ intensity.",
                                    "",
                                    colnames(protein_table))
    title <- 'Proteins Identified based on LFQ intensity'

    #Error if LFQ Intensity not found.

    if (length(protein_table) == 0) {
      print('LFQ intensities not found, changing automatically to Intensity.')

      protein_table <- proteinGroups[,grep("Intensity ",
                                           colnames(proteinGroups))]
      #Remove Intensity from name
      colnames(protein_table) <- gsub("Intensity.",
                                      "",
                                      colnames(protein_table))

      title <- 'Proteins Identified based on Intensity'
    }
  }

  #Proteins Identified
  table_prot <- data.frame(nrow(protein_table)-colSums(protein_table==0))

  rownames_prot <- rownames(table_prot)

  table_proteins <- cbind(rownames_prot,table_prot)
  #Order it
  table_proteins <- table_proteins[order(rownames(table_proteins)),]

  #Changing names
  colnames(table_proteins)[1] <- 'Experiment'
  colnames(table_proteins)[2] <- 'Proteins identified'

  #NAs
  table_proteins$'Missing values' <- nrow(protein_table) - table_proteins$`Proteins identified`


  rownames(table_proteins) <- NULL


  table_proteins <- table_proteins[, c('Experiment',
                                       'Missing values',
                                       'Proteins identified')]

  #melted
  table_melt <- melt(table_proteins, id.vars = 'Experiment')

  a <- ggplot(table_melt, aes(x=Experiment, y=value, fill=variable))+
    ggtitle(title)+
    geom_bar(stat = 'identity',position='stack',size=0.5,col="black")+
    theme(axis.title.y = element_text(margin = margin(r = 20)))+
    ylab('Number of Proteins')+
    theme_bw()+
    scale_fill_brewer(palette = palette)+
    theme(legend.position = 'bottom')

  if(long_names==TRUE){
    a + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,
                                                                     ' ',
                                                                     x),
                                                                3))
  } else{
    a
  }

}

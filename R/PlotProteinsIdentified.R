#' Title
#'
#' @param proteinGroups
#'
#' @return
#' @export
#'
#' @examples
PlotProteinsIdentified <- function(proteinGroups,long_names = FALSE, sep_names = NULL, palette = 'Set2',  font_size = 12 ){

  protein_table <- proteinGroups[,grep("Intensity ", colnames(proteinGroups))]

  #LFQ?

  # int[int == "-Inf"] <- 0
  #Remove Intensity from name
  colnames(protein_table) <- gsub("Intensity.", "", colnames(protein_table))
  #


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


  table_proteins <- table_proteins[, c('Experiment', 'Missing values', 'Proteins identified')]

  #melted
  table_melt <- melt(table_proteins, id.vars = 'Experiment')




  a <- ggplot(table_melt, aes(x=Experiment, y=value, fill=variable))+
            ggtitle('Proteins Identified')+
            geom_bar(stat = 'identity',position='stack',size=0.5,col="black")+
            theme(axis.title.y = element_text(margin = margin(r = 20)))+
            theme_bw(base_size = font_size)+
            scale_fill_brewer(palette = palette)


  if(long_names==TRUE){
    a + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
  } else{
    a
  }

}

#' Principal Component Analysis of the Intensity values.
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param intensity_type The type of intensity. Values: 'Intensity' or 'LFQ'.
#'  Default is Intensity.
#'
#' @return A PCA plot of the Intesities of all the samples.
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotPCA(proteinGroups)
#'
PlotPCA <- function(proteinGroups,
                    intensity_type = 'Intensity'){


  if (intensity_type == 'Intensity') {
    intensities <-  proteinGroups %>%  select(contains('Intensity ')& -contains('LFQ'))
    title <- 'PCA of intensity'
    colnames(intensities) <- gsub(pattern = 'Intensity', '', colnames(intensities))
  }

  if (intensity_type == 'LFQ'){
    intensities <-  proteinGroups %>%  select(contains('LFQ intensity '))
    title <- 'PCA of LFQ intensities'
    colnames(intensities) <- gsub(pattern = 'LFQ intensity', '', colnames(intensities))
    #Error if LFQ Intensity not found.

    if (length(intensities) == 1) {
      print('LFQ intensities not found, changing automatically to Intensity.')

      intensities <-  proteinGroups %>%  select(contains('Intensity ')& -contains('LFQ'))
      colnames(intensities) <- gsub(pattern = 'Intensity', '', colnames(intensities))
      title <- 'PCA of intensity'

    }

  }

  intensities_t <- t(intensities)
  #Remove columns with 0 in all the column
  intensities_t <- intensities_t[, colSums(intensities_t !=0)>0]

  pca <- stats::prcomp(intensities_t, scale = TRUE)

  df_out <- as.data.frame(pca$x)

  df_out$sample <- rownames(df_out)
  rownames(df_out) <- NULL

  ggplot(df_out, aes(PC1, PC2, color = sample))+
    geom_point(size = 3)+
    ggtitle(title)+
    theme_bw()+
    theme(legend.position = 'bottom')+
    guides(color=guide_legend(ncol=2))


}


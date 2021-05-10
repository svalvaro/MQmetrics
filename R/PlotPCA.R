#' Principal Component Analysis of the Intensity values.
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param intensity_type The type of intensity. Values: 'Intensity' or 'LFQ'.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#'  Default is Intensity.
#'
#' @return A PCA plot of the Intesities of all the samples.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotPCA(proteinGroups)
#'
PlotPCA <- function(proteinGroups,
                    intensity_type = 'Intensity',
                    palette = 'Set2'){

    PC1 <- PC2 <- Modifications <- variable <- value <- Freq <- NULL

    if (intensity_type == 'Intensity') {
        intensities <-  proteinGroups %>%  select(contains('Intensity ')& -contains('LFQ'))
        title <- 'PCA of intensity'
        colnames(intensities) <- gsub(pattern = 'Intensity',
                                      '',
                                      colnames(intensities))
    }

    if (intensity_type == 'LFQ'){
        intensities <-  proteinGroups %>%  select(contains('LFQ intensity '))
        title <- 'PCA of LFQ intensities'
        colnames(intensities) <- gsub(pattern = 'LFQ intensity',
                                      '',
                                      colnames(intensities))
        #Error if LFQ Intensity not found.

        if (length(intensities) == 1) {
            print('LFQ intensities not found, changing automatically to Intensity.')

            intensities <-  proteinGroups %>%
                select(contains('Intensity ') & -contains('LFQ'))
            colnames(intensities) <- gsub(pattern = 'Intensity',
                                          '',
                                          colnames(intensities))
            title <- 'PCA of intensity'
        }
    }

    if (length(intensities)<2 ) {
        cat('Only one sample was analyzed, PCA can not be applied')

    } else if(nrow(proteinGroups)<2){
        cat(paste0('PCA can not be performed with only ',
                   nrow(proteinGroups),
                   ' proteins.'))
    }

    else{

        intensities_t <- t(intensities)
        #Remove columns with 0 in all the column
        intensities_t <- intensities_t[, colSums(intensities_t !=0)>0]

        pca <- stats::prcomp(intensities_t, scale = TRUE)

        df_out <- as.data.frame(pca$x)

        df_out$sample <- rownames(df_out)
        rownames(df_out) <- NULL


        colourCount = length(rownames(df_out))

        getPalette = colorRampPalette(brewer.pal(8, palette))


        ggplot(df_out, aes(PC1, PC2, color = sample))+
            geom_point(size = 3)+
            ggtitle(title)+
            theme_bw()+
            theme(legend.position = 'bottom')+
            guides(color=guide_legend(ncol=2))+
            scale_color_manual(values = getPalette(colourCount))
    }
}


#' Plot PTM across samples
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param PTM_of_interest Post-Translation Modification of interest. It is
#' important they are defined exactly as MaxQuant does:
#' Examples:
#' 'Oxidation (M)', 'Acetyl (Protein N-term)', 'Unmodified', etc.
#' @param  log_base The logarithmic scale for the intensity. Default is 2.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#'
#'
#' @return A plot showing the PTM of interest.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotPTMAcrossSamples(MQCombined, PTM_of_interest = 'Oxidation (M)')
PlotPTMAcrossSamples <- function(MQCombined,
                                PTM_of_interest = 'Oxidation (M)',
                                log_base = 2,
                                long_names = FALSE,
                                sep_names = NULL){

    variable <- value <- Modifications <- NULL

    df <- MQCombined$modificationSpecificPeptides.txt

    df <- df[grepl(PTM_of_interest, df$Modifications, fixed = TRUE),]

    if(nrow(df)==0){
        message('PTM provided not found,\nDid you write it correctly?')
        return(NULL)
    }else{
        df <- df %>%  select(contains(c(
            "Modifications", "Proteins", "Intensity "))
        ) %>%
            select(-contains(c("calibrated", "Unique (Proteins)", 'Proteins')))


        df_melted <- melt(df, id.vars = 'Modifications')

        #remove the name Intensity
        df_melted$variable <- gsub('Intensity','', df_melted$variable)

        #Rremove values = 0
        df_melted <- df_melted[df_melted$value != 0, ]

        #apply log
        df_melted$value <- log(df_melted$value, base = log_base)

        # Rename the modifications, to aggrupate them into the modification of
        # interest

        df_melted$Modifications <- PTM_of_interest





        p <-    ggplot(df_melted, aes(x = variable, y = value,
                                   fill = Modifications))+
                    gghalves::geom_half_violin(side = 'r',
                                                position = position_nudge(
                                                    x = 0.25,y = 0),
                                                adjust = 2, trim = FALSE,
                                                alpha = 0.4,
                                                fill = '#FEE715FF')+
                    geom_jitter(width = 0.2, alpha = 0.1, color = '#101820FF')+
                    geom_boxplot(width = 0.07, alpha= 0.1,
                                position = position_nudge(x = 0.29, y = 0),
                                outlier.shape = NA,
                                fill = '#FEE715FF')+
                    theme_bw()+
                    ggtitle(paste0('Intensities of peptides with: ',
                                   PTM_of_interest))+
                    xlab('Experiment')+
                    ylab(paste0('Log',log_base,' of Intensity'))+
                    theme(legend.position = 'none')


        if (long_names == TRUE) {
            p <- p + scale_x_discrete(labels = function(x) {
                stringr::str_wrap(gsub(sep_names," ", x),3)})
        }

        return(p)
    }






}

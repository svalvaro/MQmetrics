#' Title
#'
#' @param MQCombined
#' @param PTM_of_interest
#' @param log_base
#' @param palette
#' @param plots_per_page
#'
#' @importFrom ggplot2 remove_missing
#'
#' @return
#' @export
#'
#' @examples
PlotPTMAcrossSamples <- function(MQCombined,
                                 PTM_of_interest = 'Oxidation (M)',
                                 log_base = 2,
                                 palette = 'Set2',
                                 plots_per_page = 5){


    df <- MQCombined$modificationSpecificPeptides.txt


    df <- df[grepl(PTM_of_interest, df$Modifications, fixed = TRUE), ]

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





        p <- ggplot(df_melted, aes(x = variable, y = value, fill = Modifications))+
            #geom_violin()#+
            gghalves::geom_half_violin()+
            gghalves::geom_half_point(shape = 21, fill = 'navyblue')+
            gghalves::geom_half_boxplot(width = 0.2)+
            theme_bw()+
            ggtitle(paste0('Intensities of peptides with: ', PTM_of_interest))+
            xlab('Experiment')+
            ylab(paste0('Log',log_base,' of Intensity'))+
            theme(legend.position = 'none')

        return(p)

    }






}

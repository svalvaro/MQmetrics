#' Identification Ratio Between Peptides and Proteins
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#'
#' @param intensity_type The type of intensity of interest. Values: 'Intensity'
#'  or 'LFQ'. Default = 'Intensity'.
#'
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#'
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#'
#' @return
#' Returns one plot showing the proteins identified vs the peptide/protein
#' ratio in each experiment.
#'
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotProteinPeptideRatio(MQCombined)
PlotProteinPeptideRatio <- function(MQCombined,
                                    intensity_type = 'Intensity',
                                    long_names = FALSE,
                                    sep_names = NULL){



    Experiment <- value <- variable <- r <- NULL

    proteinGroups <- MQCombined$proteinGroups.txt
    summary <- MQCombined$summary.txt



    if (intensity_type == 'Intensity') {


        df <- proteinGroups %>%
            select(contains('Intensity '))%>%
            select(-contains('LFQ'))

        #Remove Intensity from name
        colnames(df) <- gsub("Intensity.","",colnames(df)
        )

        title <- 'Proteins vs Peptide/Protein ratio based on Intensity'

    }

    if (intensity_type == 'LFQ'){

        #Remove LFQ Intensity from name

        df <- proteinGroups %>%select(contains('LFQ '))


        colnames(df) <- gsub("LFQ intensity.", "", colnames(df))

        title <- 'Proteins vs Peptide/Protein ratio based on LFQ'

        #Error if LFQ Intensity not found.

        if (length(df) == 0) {
            print('LFQ intensities not found,
                changing automatically to Intensity.')

            df <- proteinGroups %>%
                select(contains('Intensity ')) %>%
                select(-contains('LFQ'))

            #Remove Intensity from name
            colnames(df) <- gsub("Intensity.", "",
                                            colnames(df))

            title <- 'Proteins vs Peptide/Protein ratio based on Intensity'

        }
    }


    df <- data.frame("Proteins Identified" = nrow(df)-colSums(df==0))

    df$Experiment <- rownames(df)

    rownames(df) <- NULL




    # Add column peptides identified, and peptide/protein ratio.


    df2 <- summary %>% select(contains(c('Experiment',
                                         'Peptide Sequences Identified')))


    df_merged <- merge(df, df2, by = 'Experiment')

    df_merged$`Peptides/Proteins` <- format(round(
        df_merged$`Peptide Sequences Identified` /
            df_merged$Proteins.Identified, 1), nsmall = 1)

    df_merged$`Peptide Sequences Identified` <- NULL




     df_melt <- melt(df_merged, id.vars = 'Experiment')

     d1 <- df_melt[df_melt$variable == 'Proteins.Identified',]
     d2 <- df_melt[df_melt$variable == 'Peptides/Proteins',]

     p1 <- ggplot(d1,  aes(x = Experiment, y = as.numeric(value),
                           group = variable))+
         geom_point(colour = '#FC766AFF', size = 2)+
         geom_line(colour = '#FC766AFF', size = 1.5)+
         ggtitle(title)+
         theme_bw()+
         ylab('# Proteins Identified')+
         theme(axis.text.y=element_text(colour="#FC766AFF"))+
         theme(panel.border = element_blank(),
               panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))



     p2 <- ggplot(d2,  aes(x = Experiment, y = as.numeric(value),
                           group = variable))+
         geom_point(color = '#5B84B1FF',size = 2)+
         geom_line(colour = '#5B84B1FF',size = 1.5)+
         theme_bw()%+replace%
         theme(panel.background = element_rect(fill = NA))+
         ylab('Ratio Peptides/Proteins Identified')+
         theme(axis.text.y=element_text(colour="#5B84B1FF"))+
         theme(panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))



     if (long_names == TRUE) {
        p1 <-  p1 + scale_x_discrete(labels = function(x) {
                        stringr::str_wrap(gsub(sep_names," ", x),3)})


        p2 <-  p2 + scale_x_discrete(labels = function(x) {
            stringr::str_wrap(gsub(sep_names," ", x),3)})


     }


     # Get the ggplot grobs
     g1 <- ggplotGrob(p1)
     g2 <- ggplotGrob(p2)


     # Grab the panels from g2 and overlay them onto the panels of g1
     pp <- c(subset(g1$layout, grepl("panel", g1$layout$name), select = t:r))
     g <- gtable_add_grob(g1, g2$grobs[grepl("panel", g1$layout$name)],
                          pp$t, pp$l, pp$b, pp$l)


     # Function to invert labels
     hinvert_title_grob <- function(grob){
         widths <- grob$widths
         grob$widths[1] <- widths[3]
         grob$widths[3] <- widths[1]
         grob$vp[[1]]$layout$widths[1] <- widths[3]
         grob$vp[[1]]$layout$widths[3] <- widths[1]

         grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
         grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
         grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
         grob
     }

     # Get the y label from g2, and invert it
     index <- which(g2$layout$name == "ylab-l")
     ylab <- g2$grobs[[index]]                # Extract that grob
     ylab <- hinvert_title_grob(ylab)


     # Put the y label into g, to the right of the right-most panel
     # Note: Only one column and one y label
     g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pos = max(pp$r))

     g <-gtable_add_grob(g,ylab, t = min(pp$t), l = max(pp$r)+1,
                         b = max(pp$b), r = max(pp$r)+1,
                         clip = "off", name = "ylab-r")


     # Get the y axis from g2, reverse the tick marks and the tick mark labels,
     # and invert the tick mark labels
     index <- which(g2$layout$name == "axis-l")  # Which grob
     yaxis <- g2$grobs[[index]]                    # Extract the grob



     plot_theme <- function(p) {
         plyr::defaults(p$theme, theme_get())
     }


     # Put the y axis into g, to the right of the right-most panel
     # Note: Only one column, but two y axes - one for each
     # row of the facet_wrap plot
     g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], pos = max(pp$r))

     nrows = length(unique(pp$t)) # Number of rows
     g <- gtable_add_grob(g, rep(list(yaxis), nrows),
                          t = unique(pp$t), l = max(pp$r)-1,
                          b = unique(pp$b), r = max(pp$r)+0)

     # draw it
     return(grid.draw(g))
}

#' Post-Translational Modifications
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param peptides_modified Minimum number of peptides modified. Default  is 5.
#' @param palette The palette from the Package RColorBrewer. By default is
#' 'Set2'.
#' @param plot_unmodified_peptides If TRUE, it will show the Unmodified
#' peptides.
#' @param log_base The logarithmic scale for the intensity. Default is 2.
#' @param aggregate_PTMs If TRUE, same PTM that occur multiple times in the
#'  same peptides,  will be aggregated together.
#' @param combine_same_residue_ptms Combine the PTMs that happen in the same
#' residue such as Dimethyl (KR), Trimethyl (KR) into only one group:
#' Methyl (KR).
#' @param plots_per_page Establish the maximum number of plots per page.
#'
#' @return Two plots per sample
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotPTM(MQCombined)
PlotPTM <- function(MQCombined,
                    peptides_modified = 1,
                    plot_unmodified_peptides = FALSE,
                    log_base = 2,
                    aggregate_PTMs = TRUE,
                    combine_same_residue_ptms = TRUE,
                    palette = "Set2",
                    plots_per_page = 5) {

    modificationSpecificPeptides <- MQCombined$modificationSpecificPeptides.txt

    Modifications <- variable <- value <- Freq <- sample_num <- NULL
    Mod2 <- frequency2 <- NULL

    modification_table <- modificationSpecificPeptides %>%
        select(contains(c(
            "Modifications", "Proteins",
            "Intensity ", "Experiment "
        ))) %>%
        select(-contains(c("calibrated", "Unique (Proteins)")))

    mod_melted <- modification_table %>% select(-contains('Intensity'))


    mod_melted <- melt(mod_melted, id.vars = c("Modifications", "Proteins"))

    # table with the modification, the proteins, the experiment, and the
    # frequency.
    mod_melted[is.na(mod_melted)] <- 0


    # Frequency of each modification

    mod_frequencies <- mod_melted %>%
        group_by(Modifications, variable) %>%
        summarise(Freq = sum(value))

    # Separate the peptides containing multiple modifications:

    df <- mod_frequencies %>%
        separate_rows(Modifications, sep = ";") %>%
        group_by(Modifications, variable) %>%
        summarise(Freq = sum(Freq))

    # Remove the Experiment pattern from the variable

    df$variable <- gsub("Experiment", "", df$variable)

    # Remove the peptides with less frequency than the peptides modified
    df <- df[df$Freq >= peptides_modified, ]


    if (plot_unmodified_peptides == FALSE) {
        df <- df[!df$Modifications == "Unmodified", ]
    }



    legend_title = 'All PTMs'

    if(aggregate_PTMs == TRUE){


        # Aggregate together PTMs like
        # 2 oxidation, 3 oxidation, 2 acetylation ...

        mod_join <- df

        # Obtain the row numbers of peptides with multiple modifications
        indexes <- which( grepl('[0-9]',mod_join$Modifications))

        df <- mod_join[indexes,]

        mod_final <- mod_join[-c(indexes),] # removed indexes

        combined <- df %>%
            mutate(Mod2 = str_replace_all(Modifications, "[:digit:]", "") %>%
                       str_squish(),
                   sample_num = as.numeric(gsub("\\D", "", Modifications)),
                   frequency2 = sample_num * Freq) %>%
            group_by(variable,Mod2) %>%
            summarise(Freq = sum(frequency2))

        colnames(combined)[colnames(combined) == "Mod2"] <- 'Modifications'

        combined_final <- rbind(mod_final, combined)

        df <- combined_final %>%
            group_by(Modifications, variable) %>%
            summarise(Freq = sum(Freq))

        # Change the legend title

        legend_title = 'Aggregated PTMs'

    }

    if (combine_same_residue_ptms == TRUE) {

        # Also needs to be aggregated PTMs like mono, di, Tri

        #Find the Di- modifications
        di_indexes <- which(grepl('^Di', df$Modifications))


        #Find the Tri- modifications
        tri_indexes <- which(grepl('^Tri', df$Modifications))


        # If there are not Di- this won't be run
        if (length(di_indexes) > 0) {
            # Multiply the frequency of the indexes with Di  by 2
            # we will leave it for now as 1 modification

            df$Freq[di_indexes] <- df$Freq[di_indexes] * 1

            # Remove Di- from the name. And make the first letter in cap
            df$Modifications[di_indexes] <- gsub('^Di','',
                                                 df$Modifications[di_indexes])
            df$Modifications[di_indexes] <- tools::toTitleCase(
                df$Modifications[di_indexes])

            # Do the same for tri modifications
        }

        # If there are not Tri- this won't be run
        if (length(tri_indexes) > 0) {


            # Do the same for tri modifications

            # Multiply the frequency of the indexes with Tri  by 3
            # we will leave it for now as 1 modification
            df$Freq[tri_indexes] <- df$Freq[tri_indexes] * 1


            # Remove Tri- from the name. And make the first letter in cap
            df$Modifications[tri_indexes] <- gsub('^Tri','',
                                                  df$Modifications[tri_indexes])
            df$Modifications[tri_indexes] <- tools::toTitleCase(
                df$Modifications[tri_indexes])
        }


        # Finally aggregate the df table

        df <- df %>%
                group_by(Modifications, variable) %>%
                summarise(Freq = sum(Freq))
    }

    # For Intensity plot

    modifications_unique <- unique(df$Modifications) # names

    mod_intensities <- modification_table %>%
        select(-contains("Experiment"))

    mod_intensities <- melt(mod_intensities,
                            id.vars = c("Modifications", "Proteins")
    )

    mod_intensities <- mod_intensities[mod_intensities$value != 0, ]

    # Select only the same modifications as in the Frequency
    mod_intensities2 <- mod_intensities[mod_intensities$Modifications %in%
                                            modifications_unique, ]

    mod_intensities2$variable <- gsub("Intensity","",mod_intensities2$variable)


    if (log_base == 2) {
        mod_intensities2$value <- log2(mod_intensities2$value)
        ylab <- expression("Log"[2] * "(Intensity)")
    } else if (log_base == 10) {
        mod_intensities2$value <- log10(mod_intensities2$value)
        ylab <- expression("Log"[10] * "(Intensity)")
    }

    # samples for paginate

    n_samples <- length(modification_table %>% select(contains("Experiment")))

    n_pages_needed <- ceiling(
        n_samples / plots_per_page
    )

    colourCount <- n_samples

    getPalette <- colorRampPalette(brewer.pal(8, palette))

    myplots <- list() # initate a list to store the plots


    for (ii in seq_len(n_pages_needed)) {
        if (n_samples < plots_per_page) {
            nrow <- n_samples
        } else {
            nrow <- plots_per_page
        }

        a <- ggplot(df, aes(
            x = Modifications,
            y = Freq,
            fill = Modifications
        )) +
            geom_bar(stat = "identity") +
            facet_wrap_paginate(. ~ variable, ncol = 1,
                                nrow = nrow,
                                page = ii) +
            theme_bw() +
            ggtitle("Frequency of modified peptides") +
            ylab("Peptide Frequency") +
            theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
            guides(fill = guide_legend(ncol = 3)) +
            scale_fill_brewer(palette = palette)+
            labs(fill = legend_title)


        b <- ggplot(mod_intensities2, aes(
            x = Modifications,
            y = value,
            color = Modifications
        )) +
            geom_violin(fill = "gray80", size = 1, alpha = .5) +
            geom_boxplot(width = 0.2) +
            facet_wrap_paginate(. ~ variable,
                                ncol = 1,
                                nrow = nrow,
                                page = ii) +
            theme_bw() +
            ggtitle("Intensities of modified peptides") +
            ylab(ylab) +
            theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            scale_colour_brewer(palette = palette)


        c <- plot_grid(a + theme(legend.position = "none"),
                                b + theme(legend.position = "none"),
                                ncol = 2, rel_heights = c(0.1, 1)
        )
        title <- ggdraw() + draw_label("Post-Translational Modifications")

        prow <- plot_grid(title, c, ncol = 1, rel_heights = c(0.1, 1))

        legend <- get_legend(a)



        myplots[[ii]] <-  plot_grid(prow, legend, ncol = 1,
                                    rel_heights = c(9, 1))
    }
    return(myplots)
}

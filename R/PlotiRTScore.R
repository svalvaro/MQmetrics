#' Score vs retention time of the iRT peptides
#'
#' @param MQCombined Object list containing all the files from the MaxQuant
#' output. It is the result from using \code{make_MQCombined}.
#' @param tolerance Error maximum to find the iRT peptides by m/z value.
#'  by default is 0.001.
#' @param plots_per_page Establish the maximum number of plots per page.#'
#'
#' @return A plot for each sample showing a linear regression of the
#' iRT peptides'
#'  retention time vs the score.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")
#' MQCombined <- make_MQCombined(MQPathCombined)
#' PlotiRT(MQCombined)
PlotiRTScore <- function(MQCombined,
                        tolerance = 0.001,
                        plots_per_page = 5) {
    evidence <- MQCombined$evidence.txt

    Experiment <- `m/z` <- `Retention time` <- Sequence <- Intensity <- NULL

    iRT.mZ <- c(
        487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
        683.8282, 683.8541, 699.3388, 726.8361, 776.9301
    )

    iRT.score <- c(
        -24.92, 19.79, 70.52, 87.23, 0, 28.71, 12.39, 33.38, 42.26,
        54.62, 100
    )

    names(iRT.mZ) <- names(iRT.score) <- Sequence <- c(
        "LGGNEQVTR",
        "YILAGVENSK",
        "GTFIIDPGGVIR",
        "GTFIIDPAAVIR",
        "GAGSSEPVTGLDAK",
        "TPVISGGPYEYR",
        "VEATFGVDESNAK",
        "TPVITGAPYEYR",
        "DGLDAASYYAPVR",
        "ADVTPADFSEWSK",
        "LFLQFGAQGSPFLK"
    )
    names_Sequence <- names(Sequence) <- c(
        "iRT Kit_a",
        "iRT Kit_d",
        "iRT Kit_i",
        "iRT Kit_k",
        "iRT Kit_b",
        "iRT Kit_e",
        "iRT Kit_c",
        "iRT Kit_f",
        "iRT Kit_g",
        "iRT Kit_h",
        "iRT Kit_l"
    )


    iRT_score <- data.frame(iRT.score, Sequence)

    irt_names_table <- data.frame(Sequence, names_Sequence)

    # obtain the indeces with the iRT peptides sequences in the evidence table.

    indexes_prot <- which(evidence$Sequence %in% Sequence)

    # if no iRT peptide found return error.
    if (length(indexes_prot) == 0) {
        print("No iRT peptides found in the MaxQuant output.")
    } else {

        # obtain rows only with irt by sequence
        iRT_table_prot <- evidence[indexes_prot, ]

        # remove rows with NA in intensity
        iRT_table_prot <- iRT_table_prot[complete.cases(
            iRT_table_prot$Intensity), ]

        # make table smaller
        iRT_table_prot <- iRT_table_prot %>% select(c(
            Experiment, `m/z`,
            `Retention time`,
            Sequence, Intensity
        ))

        # from the irt obtained, filter them by the theoretical m/z with
        # tolerance
        # in_range <- unlist(vapply(iRT_table_prot$`m/z`,
        #                         function(x) x[any(abs(x- iRT.mZ)
        # < tolerance)]))

        in_range <- unlist(sapply(
            iRT_table_prot$`m/z`,
            function(x) x[any(abs(x - iRT.mZ) < tolerance)]
        ))

        # Obtain the indexes and final table
        indexes <- which(iRT_table_prot$`m/z` %in% in_range)


        iRT_table_prot_final <- iRT_table_prot[indexes, ]

        # iRT_table_prot_final$score <- NA

        # Add a new column score
        iRT_table_prot_final <- merge(iRT_table_prot_final,
                                    iRT_score,
                                    by = "Sequence"
        )

        # Add a new column names
        iRT_table_prot_final <- merge(iRT_table_prot_final,
                                    irt_names_table,
                                    by = "Sequence"
        )

        # obtain the maximum intensity values for each experiment, and sequence.
        iRT_table_prot_maxvalues <- iRT_table_prot_final %>%
            group_by(Experiment, Sequence) %>%
            filter(Intensity== max(Intensity))

        # Create a function that plots the linear regression with the data
        lm_eqn <- function(df) {
            y <- df$`Retention time`
            x <- df$iRT.score

            m <- lm(y ~ x, df)
            eq <- substitute(
            italic(y) == a + b %.% italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
            list(
                a = format(unname(coef(m)[1]), digits = 2),
                b = format(unname(coef(m)[2]), digits = 2),
                r2 = format(summary(m)$r.squared, digits = 3)
            )
        )
            as.character(as.expression(eq))
        }


        n_samples <-length(unique(iRT_table_prot_maxvalues$Experiment))

        n_pages_needed <- ceiling(n_samples / plots_per_page)


        myplots <- list()
        for (ii  in seq_len(n_pages_needed)) {

            if (n_samples < plots_per_page) {
                nrow <- n_samples
            }else{
                nrow <- plots_per_page
            }



            # plot it.
            p <- ggscatter(iRT_table_prot_maxvalues,
                    x = "iRT.score",
                    y = "Retention time",
                    add = "reg.line") +
                theme_bw() +
                geom_vline(xintercept = 0, size = 0.5, linetype = 2) +
                stat_cor(label.x = 3, label.y = 120) +
                stat_regline_equation(label.x = 3, label.y = 110) +
                facet_wrap_paginate(~Experiment, page = ii, nrow = nrow) +
                geom_point(aes(fill = names_Sequence),
                            shape = 21,
                            colour = "black", size = 3) +
                ggtitle(label = "Retention time ofthe Biognosys iRT peptides.")+
                labs(fill = 'iRT peptides')+
                theme(legend.position = "bottom")

            myplots[[ii]] <- p
        }


    }
    return(myplots)
}

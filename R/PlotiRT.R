#' Max intensities of the iRT peptides in each sample.
#'
#' @param evidence evidence.txt table from the MaxQuant ouptut.
#' @param show_calibrated_rt If TRUE, it will also show the calibrated retention
#'  time of each iRT peptide. By default = FALSE.
#' @param tolerance Error maximum to find the iRT peptides by m/z value.
#'  by default is 0.001.
#'
#' @return A plot showing the iRT peptide in each sample vs the Retention time.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' evidence <- files[['evidence.txt']]
#' PlotiRT(evidence)
#'
PlotiRT <- function(MQCombined,
                    show_calibrated_rt = FALSE,
                    tolerance=0.001){
    evidence <- MQCombined$evidence.txt

    Experiment <- `m/z` <- `Retention time` <- Sequence <- Intensity <- NULL
    `Calibrated retention time` <- value <- variable <- NULL

    iRT.mZ <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
                683.8282, 683.8541, 699.3388, 726.8361, 776.9301)


    names(iRT.mZ) <- Sequence <- c("LGGNEQVTR",
                                   "YILAGVENSK",
                                   "GTFIIDPGGVIR",
                                   "GTFIIDPAAVIR",
                                   "GAGSSEPVTGLDAK",
                                   "TPVISGGPYEYR",
                                   "VEATFGVDESNAK",
                                   "TPVITGAPYEYR",
                                   "DGLDAASYYAPVR",
                                   "ADVTPADFSEWSK",
                                   "LFLQFGAQGSPFLK")


    names_Sequence <- names(Sequence) <- c('iRT Kit_a',
                                           'iRT Kit_d',
                                           'iRT Kit_i',
                                           'iRT Kit_k',
                                           'iRT Kit_b',
                                           'iRT Kit_e',
                                           'iRT Kit_c',
                                           'iRT Kit_f',
                                           'iRT Kit_g',
                                           'iRT Kit_h',
                                           'iRT Kit_l')

    irt_names_table <- data.frame(Sequence, names_Sequence)
    #Check for the iRT peptides by sequence
    indexes_prot <-  which(evidence$Sequence %in% Sequence)

    #if no iRT peptide found return error.
    if (length(indexes_prot)==0) {

        print('No iRT peptides found in the MaxQuant output.')

    } else{

        #obtain rows only with irt by sequence
        iRT_table_prot <- evidence[indexes_prot,]

        #remove rows with NA in intensity
        iRT_table_prot <- iRT_table_prot[complete.cases(iRT_table_prot$Intensity),]

        #make table smaller
        iRT_table_prot <- iRT_table_prot %>% select(c(Experiment,
                                                      `m/z`,
                                                      `Retention time`,
                                                      `Calibrated retention time`,
                                                      Sequence,
                                                      Intensity))

        #from the irt obtained, filter them by the theoretical m/z with tolerance
        in_range <- unlist(sapply(iRT_table_prot$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)]))
        #in_range <- unlist(vapply(iRT_table_prot$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)],))



        #Obtain the indexes and final table
        indexes <- which(iRT_table_prot$`m/z` %in% in_range)


        iRT_table_prot_final  <- iRT_table_prot[indexes,]

        iRT_table_prot_final <- merge(iRT_table_prot_final,
                                      irt_names_table,
                                      by = "Sequence")

        #obtain the maximum intensity values for each experiment, and sequence.
        iRT_table_prot_maxvalues <- iRT_table_prot_final %>%
            group_by(Experiment, Sequence) %>%
            filter(Intensity
                   == max(Intensity))

        b <- ggplot(iRT_table_prot_maxvalues,aes(y = Intensity,
                                                 colour = names_Sequence))+
            # colour = as.character(`m/z`)))+
            geom_point(aes(x = `Retention time`), size = 2)+
            geom_segment(aes(x = `Retention time`, xend=`Retention time`, yend=0))+
            facet_wrap(. ~ Experiment, ncol= 1)+
            ggtitle('Biognosys iRT peptides in each sample.')+
            theme_bw()+
            labs(colour='iRT peptides')+
            theme(legend.position = 'bottom')

        if(show_calibrated_rt == TRUE){
            irt_melted <- melt(iRT_table_prot_maxvalues,
                               id.vars = c('Sequence', 'Experiment','m/z',
                                           'names_Sequence', 'Intensity'))
            ggplot(irt_melted, aes(x = value,
                                   y = Intensity,
                                   colour = names_Sequence))+
                geom_point(aes(shape = variable), size = 2)+
                geom_segment(aes(x = value, xend=value, yend=0))+
                facet_wrap(. ~ Experiment, ncol= 1)+
                ggtitle('Biognosys iRT peptides in each sample.')+
                theme_bw()+
                labs(colour='iRT peptides')+
                theme(legend.position = 'bottom')

        }else{
            b
        }
    }
}

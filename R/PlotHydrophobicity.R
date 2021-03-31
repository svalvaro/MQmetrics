#' Peptide hydrophobicity by GRAVY score
#'
#' @param peptides The peptides file from the MaxQuant ouptut.
#' @param show_median If true it will show the median of each group, as a red
#'  dashed line.By default is TRUE.
#' @param binwidth Selects the binwidth of the histogram. By default = 0.2
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Returns a histogram per sample, showing the frequency of the peptide's
#'  hydrophobicity GRAVY value.
#'
#' @export
#'
#' @examples
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' PlotHydrophobicity(peptides)
#'
PlotHydrophobicity <- function(peptides,
                      show_median = TRUE,
                      binwidth = 0.2,
                      palette = 'Set2'){

  df <- peptides %>%  select(contains(c('Length',"Count","Sequence","Experiment")))

  df$GRAVY <-  (df$`A Count` * 1.8 +
                df$`R Count` * -4.5 +
                df$`N Count` * -3.5 +
                df$`D Count` * -3.5 +
                df$`C Count` * 2.5 +
                df$`Q Count` * -3.5 +
                df$`E Count` * -3.5 +
                df$`G Count` * -0.4 +
                df$`H Count` * -3.2 +
                df$`I Count` * 4.5 +
                df$`L Count` * 3.8 +
                df$`K Count` * -3.9 +
                df$`M Count` * 1.9 +
                df$`F Count` * 2.8 +
                df$`P Count` * -1.6 +
                df$`S Count` * -0.8 +
                df$`T Count` * -0.7 +
                df$`W Count` * -0.9 +
                df$`Y Count` * -1.3 +
                df$`V Count` * 4.2)/df$Length


  df <- df %>% select(contains(c('GRAVY', 'Experiment')))

  df_out <- melt(df, id.vars = 'GRAVY')

  df_out$variable <- gsub('Experiment ', '', df_out$variable)

  #Remove value 0,

  df_out<-df_out[!is.na(df_out$value),]


  #Repeat rows n numbers of times, being n the frequency (value)
  df_expanded<- df_out[rep(rownames(df_out),df_out$value),]





a <-  df_expanded %>%
          group_by(variable) %>%
          ggplot(aes(x = GRAVY, fill = variable, group = variable))+
          geom_histogram(color = 'black', binwidth = binwidth )+
          facet_wrap(.~ variable, ncol =1)+
          theme_bw()+
          theme(legend.position = 'none')+
          scale_fill_brewer(palette = palette)+
          ggtitle('Peptide hydropathy distribution')+
         ylab('Peptide Frequency')+
         xlab('GRAVY score')


  if (show_median ==TRUE) {

    median_groups <-  df_expanded %>%
      group_by(variable) %>%
      summarise(median(GRAVY))

    a + geom_vline(data = median_groups,
                   aes(xintercept = `median(GRAVY)`,group = variable),
                   color = 'red',  linetype = 'dashed')

  } else{

    a
  }




}




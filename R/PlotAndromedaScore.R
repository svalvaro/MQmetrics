#' Andromeda score for the best associated MS/MS spectrum.
#'
#' @param peptides peptides.txt table from MaxQuant ouput.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Plots the MaxQuant Andromeda Score.
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' peptides <- files[['peptides.txt']]
#' PlotAndromedaScore(peptides)
PlotAndromedaScore <- function(peptides,
                               palette = 'Set2'){

  variable <- Score <- NULL

  df <- peptides %>%  select(contains(c('id', 'Score', 'Experiment'), ignore.case = FALSE)) %>%
        select(-contains(c('acid','peptide','IDs')))


  df_out <- melt(df, id.vars = c('id', 'Score'))

  df_out$variable <- gsub('Experiment', '', df_out$variable)

  #Remove missing values

  df_out <- df_out[!is.na(df_out$value),]


  #Repeat rows n numbers of times, being n the frequency (value)
  df_expanded<- df_out[rep(rownames(df_out), df_out$value),]


  colourCount = length(df)-2

  getPalette = colorRampPalette(brewer.pal(8, palette))

  n_pages_needed <- ceiling(
    (colourCount)/ 5
  )


  for (ii in seq_len(n_pages_needed)) {

    if (colourCount < 5) {
      nrow = colourCount

    } else{
      nrow = 5
    }


    p <- df_expanded %>%
      group_by(variable) %>%
      ggplot(aes(x = Score, fill = variable))+
            geom_histogram(color = 'black', binwidth = 5 )+
            facet_wrap_paginate(. ~ variable, ncol = 1, nrow = nrow, page = ii )+
            theme_bw()+
            ylab('Peptide Frequency')+
            ggtitle(label = 'Andromeda score')+
            scale_fill_manual(values = getPalette(colourCount))+
            theme(legend.position = 'none')

    print(p)



  }


}

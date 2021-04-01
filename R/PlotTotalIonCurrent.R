#' Total Ion Current
#'
#' @param msmsScans The msmsScans.txt table from  MaxQuant Output.
#' @param show_max_value If TRUE, it will show the max TIC value of each sample.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return Returns a plot the Total Ion Current in each sample. The maximum value is also plotted.
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' msmsScans <- files[['msmsScans.txt']]
#' PlotTotalIonCurrent(msmsScans)
#'
#'
PlotTotalIonCurrent <- function(msmsScans,
                                show_max_value = TRUE,
                                palette = 'Set2'){

  `Retention time` <- `Total ion current` <- Experiment <- . <- NULL

  n_samples <- length(unique(msmsScans$Experiment))

  n_pages_needed <- ceiling(
    n_samples / 5
  )

  for(ii in seq_len(n_pages_needed)){
    if(n_samples < 5){
      nrow = n_samples
    } else{
      nrow = 5
    }


    p <- msmsScans %>%   ggplot(aes(`Retention time`,`Total ion current`))+
      geom_line(aes(colour=Experiment))+
      facet_wrap_paginate(.~ Experiment, ncol =1, nrow = nrow, page = ii)+
      ggtitle('Total Ion Current')+
      theme_bw()+
      theme(legend.position = 'none')+
      scale_colour_brewer(palette = palette)


    if (show_max_value==TRUE) {
      print(p + geom_label(data = . %>% group_by(Experiment) %>% filter(`Total ion current`== max(`Total ion current`)),
                     aes(label= format(`Total ion current`, digits = 2, scientific = TRUE)), hjust=0.5))
    }else{
      print(p)
    }

  }


}


#' The charge-state of the precursor ion.
#'
#' @param evidence The evidence.txt file from the MaxQuant ouptut.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#'
#' @return Plots the charge-state of the precursor ion.
#' @export
#'
#' @examples
#' MQPathCombined <- '/home/alvaro/Documents/MaxQuant/example4/'
#' files <- ReadDataFromDir(MQPathCombined)
#' evidence <- files[['evidence.txt']]
#' PlotCharge(evidence)
#'
#'
PlotCharge <- function(evidence,
                       palette = 'Set2'){
  Experiment <- Charge <-  value <- variable <- NULL

  ev <- evidence %>%
    select(c(Experiment,Charge ))

  ev_agrup <- dcast(ev, Experiment~ Charge, fill = 0)


  ev_agrup_m <- melt(ev_agrup, id.vars = "Experiment")


  n_pages_needed <- ceiling(
    length(unique(ev_agrup$Experiment))/ 5
  )



 for (ii in seq_len(n_pages_needed)) {

   if(length(unique(ev_agrup$Experiment)) <5){
     nrow = length(unique(ev_agrup$Experiment))
   } else{
     nrow = 5
   }


  p <- ggplot(ev_agrup_m, aes(x = variable, y = value , fill = Experiment)) +
           geom_bar(stat='identity', color = 'black')+
           facet_wrap_paginate(.~ Experiment, ncol =1, nrow = nrow, page = ii)+
           ggtitle(label = 'The charge-state of the precursor ion.')+
           theme(legend.position = 'none')+
           xlab(label = 'Charge')+
           theme_bw()+
           theme(legend.position='none')+
           scale_fill_brewer(palette = palette)

  print(p)

 }



}

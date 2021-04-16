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
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' evidence <- files[['evidence.txt']]
#' PlotCharge(evidence)
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


  colourCount = length(unique(ev$Experiment))

  getPalette = colorRampPalette(brewer.pal(8, palette))



 for (ii in seq_len(n_pages_needed)) {

   if(colourCount <5){
     nrow = colourCount
   } else{
     nrow = 5
   }


  p <- ggplot(ev_agrup_m, aes(x = variable, y = value, fill = Experiment)) +
           geom_bar(stat='identity', color = 'black')+
           scale_fill_manual(values = getPalette(colourCount))+
           facet_wrap_paginate(.~ Experiment, ncol =1, nrow = nrow, page = ii)+
           ggtitle(label = 'The charge-state of the precursor ion.')+
           theme(legend.position = 'none')+
           xlab(label = 'Charge')+
           theme_bw()+
           theme(legend.position='none')

  print(p)

 }



}

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
#' MQPathCombined <- system.file('extdata', package = 'MQmetrics')
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

ggplot(ev_agrup_m, aes(x = variable, y = value , fill = Experiment)) +
  geom_bar(stat='identity', color = 'black')+
  facet_wrap(.~ Experiment, ncol =1)+
  ggtitle(label = 'The charge-state of the precursor ion.')+
  theme(legend.position = 'none')+
  xlab(label = 'Charge')+
  theme_bw()+
  theme(legend.position='none')+
  scale_fill_brewer(palette = palette)



}

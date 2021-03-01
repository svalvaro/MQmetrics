#From Evidence_table

#' The charge-state of the precursor ion.
#'
#' @param evidence The evidence.txt file from the MaxQuant ouptut.
#'
#'
#' @return Plots the charge-state of the precursor ion.
#' @export
#'
#' @examples
PlotCharge <- function(evidence, font_size = 12){
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
  theme_bw(base_size = font_size)+
  theme(legend.position='none')



}

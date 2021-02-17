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
PlotCharge <- function(evidence){
  Experiment <- Charge <-  value <- variable <- NULL

  ev <- evidence %>%
    select(c(Experiment,Charge ))

  ev_agrup <- dcast(ev, Experiment~ Charge, fill = 0)

  # ev_perc <- ev_agrup[,-1]/rowSums(ev_agrup[,-1])*100
  #
  # ev_perc <- cbind(ev_agrup$Experiment, ev_perc)
  #
  #
  # names(ev_perc)[1] <- 'Experiment'
  #
  # ev_bubbles <- melt(ev_perc, id.vars = 'Experiment')
  #
  # ggplot(ev_bubbles, aes(x = Experiment, y = variable )) +
  #   geom_point(aes(size=value,fill= variable),alpha=0.75, shape = 21) +
  #   guides(fill=FALSE)+
  #   scale_size_continuous(limits = c(0.000001,100), range = c(0,10), breaks = c(1,5,10, 20, 50,75)) +
  #   labs( x= "", y = "", size = "Relative Abundance (%)", fill = "")  +
  #   theme(legend.key=element_blank(),
  #         axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1),
  #         axis.text.y = element_text(colour = "black", face = "bold", size = 11),
  #         legend.text = element_text(size = 10, face ="bold", colour ="black"),
  #         legend.title = element_text(size = 12, face = "bold"),
  #         panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
  #         legend.position = "right")+
  #   ggtitle(label = 'Charge')
  ev_agrup_m <- melt(ev_agrup, id.vars = "Experiment")

  ggplot(ev_agrup_m, aes(x = variable, y = value , fill = Experiment)) +
    geom_bar(stat='identity', color = 'black')+
    facet_grid(Experiment~.)+
    ggtitle(label = 'The charge-state of the precursor ion.')+
    theme(legend.position = 'none')+
    xlab(label = 'Charge')


}

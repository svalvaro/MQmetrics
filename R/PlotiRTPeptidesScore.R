#' Title
#'
#' @param msmsScans
#'
#' @return
#' @export
#'
#' @examples
PlotiRTScore <- function(evidence){

  iRT.mZ <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
              683.8282, 683.8541, 699.3388, 726.8361, 776.9301)

  iRT.score <- c(-24.92, 19.79, 70.52, 87.23, 0, 28.71, 12.39, 33.38, 42.26,
                 54.62, 100)

  names(iRT.mZ) <- names(iRT.score) <- Sequence <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                     "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                     "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                     "LFLQFGAQGSPFLK")

  iRT_score <- data.frame(iRT.score, Sequence)



    tolerance <- 0.001



  indexes_prot <-  which(evidence$Sequence %in% Sequence)

  iRT_table_prot <- evidence[indexes_prot,]

  #remove rows with NA in intensity
  iRT_table_prot <- iRT_table_prot[complete.cases(iRT_table_prot$Intensity),]


  iRT_table_prot <- iRT_table_prot %>% select(c(Experiment,`m/z`,`Retention time`,
                                                Sequence, Intensity))


  in_range <- unlist(sapply(iRT_table_prot$`m/z`, function(x) x[any(abs(x- iRT.mZ) < tolerance)]))

  indexes <- which(iRT_table_prot$`m/z` %in% in_range)


  iRT_table_prot_final  <- iRT_table_prot[indexes,]

  #iRT_table_prot_final$score <- NA


  iRT_table_prot_final <- merge(iRT_table_prot_final, iRT_score, by = "Sequence")

  iRT_table_prot_maxvalues <- iRT_table_prot_final %>%
    group_by(Experiment, Sequence) %>%
    filter(Intensity
           == max(Intensity))


  lm_eqn <- function(df){
    y = df$`Retention time`
    x = df$iRT.score

    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }



# ggplot(iRT_table_prot_maxvalues, aes(x = iRT.score, y = `Retention time`))+
#   geom_smooth(method='lm', formula =y ~ x , color ='grey', se = FALSE)+
#   geom_point(aes(colour = Sequence))+
#   facet_wrap(Experiment ~ .)+
#   geom_text(label = lm_eqn(iRT_table_prot_maxvalues))+
#   ggtitle('Biognosys iRT peptides in each sample.')+
#   theme(legend.position = 'bottom')

ggscatter(iRT_table_prot_maxvalues, x = "iRT.score", y = "Retention time",
          add = 'reg.line')+
  geom_point(aes(colour = Sequence))+
  theme_bw()+
  geom_vline(xintercept = 0, size = 0.5, linetype = 2)+
  stat_cor(label.x = 3, label.y = 120) +
  stat_regline_equation(label.x = 3, label.y = 110)+
  facet_wrap(~Experiment)+
  theme(legend.position =  'bottom')



}

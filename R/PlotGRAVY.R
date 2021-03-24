#' Title
#'
#' @param peptides
#' @param palette
#'
#' @return
#' @export
#'
#' @examples
PlotGRAVY <- function(peptides, palette = 'Set2'){
  df <- peptides %>%  select(contains(c('Length',"Count","Sequence","Experiment")))


#
#     GRAVY_values <- c("A"= "1.8", "R"= "-4.5", "N"= "-3.5",
#                       "D"= "-3.5", "C"= "2.5", "Q"= "-3.5",
#                       "E"= "-3.5", "G"= "-0.4", "H"= "-3.2",
#                       "I"= "4.5", "L"= "3.8", "K"= "-3.9",
#                       "M"= "1.9", "F"= "2.8", "P"= "-1.6",
#                       "S"= "-0.8", "T"= "-0.7", "W"= "-0.9",
#                      "Y"= "-1.3", "V"= "4.2")


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



  ggplot(df_out, aes(x=  GRAVY,variable ,color = variable))+
    geom_density_ridges(stat = 'density', aes(height = ..count..), alpha = 0.5)+
    ggtitle('Peptide hidropathy distribution')+
    theme_ridges()+
    theme(legend.position = 'none')+
    scale_colour_brewer(palette = palette)


  # ggplot(df_out, aes(x=  GRAVY, color = variable))+
  #   geom_density(aes(y = ..count..), alpha = 0.5)+
  #   facet_wrap(.~ variable, ncol =1)+
  #   ggtitle('Peptide hidropathy distribution')+
  #   theme_bw()



}




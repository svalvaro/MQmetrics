#Plot the dynamic range of all the samples combined


#' Plots the dynamic range for all samples
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param  columns The number of column to show the output.
#' @param rows The number of rows to show the output.
#'
#' @return Returns one plot for each sample, being the dynamic range. Besides, it can shows where 90\% of the proteins are and their orders of abundance.
#' @export
#'
#' @examples
PLotAllDynamicRange <- function(proteinGroups, columns =1, rows=1){



  rank_groups<-  proteinGroups %>%  select(contains("Intensity ")) %>% select(-starts_with('LFQ'))

  rank_groups <- log10(rank_groups)

  pl <- vector("list", length = ncol(rank_groups))

  for(i in 1:ncol(rank_groups))
  {temp <- data.frame(rank_groups[,i])
  colnames(temp) <- colnames(rank_groups)[i]
  assign(colnames(rank_groups)[i], temp)


  temp <- temp[order(temp[,1], decreasing=TRUE), ]

  temp <- temp[!grepl('^-Inf$', temp)]

  vec_temp <- seq(1:length(temp))

  temp_data <- data.frame(vec_temp, temp)


  values_5_percent <- round(nrow(temp_data)*0.05)

  ymin_temp = temp_data$temp[values_5_percent]

  ymax_temp = temp_data$temp[nrow(temp_data)-values_5_percent]

  orders_abundance_temp <- paste(round(ymin_temp-ymax_temp,digits = 1), 'orders \n of abundance')


  temp_plot <- ggplot(temp_data, aes(x=vec_temp,y = temp))+

    geom_point(colour='darkgrey', alpha=0.75, shape=21)+

    ggtitle(colnames(rank_groups)[i])+

    theme_bw()+

    ylab(expression('log'[10]*'(Intensity)'))+

    xlab('Protein Abundance Rank')#+
#
#     annotate("errorbar", x = nrow(temp_data)/2, y =(ymax_temp+ymin_temp)/2, xmin = values_5_percent, xmax =
#                nrow(temp_data)-values_5_percent,  colour = "black", size = 1.0)+
#
#     annotate(geom="text", x=values_5_percent +0.1*nrow(temp_data), y=(ymax_temp+ymin_temp)/2-0.07*ymin_temp, label="90% \n proteins",
#              size=3)+
#
#     annotate("errorbar", x = nrow(temp_data)/2, y = (ymax_temp+ymin_temp)/2 , ymin = ymin_temp,
#              ymax = ymax_temp ,  colour = "black", size = 1.0, width=0.1*nrow(rank))+
#
#     annotate(geom="text", x=nrow(temp_data)/2 +0.15*nrow(temp_data), y=ymin_temp-0.05*ymin_temp, label=orders_abundance_temp, size=3)



  pl[[i]] <- temp_plot



  rm(temp)
  rm(vec_temp)
  rm(values_5_percent)
  rm(ymin_temp)
  rm(ymax_temp)
  rm(orders_abundance_temp)

  }

  marrangeGrob(grobs=pl, ncol=columns, nrow = rows, top = NULL)



}




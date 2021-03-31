#Plot the dynamic range of all the samples combined


#' Plots the dynamic range for all samples
#'
#' @param proteinGroups The proteinGroups.txt table from  MaxQuant Output.
#' @param show_shade Creates a shade showing where the \code{percent_proteins} are.
#'  Default is TRUE.
#' @param percent_proteins Determines the percentage for the show_shade parameter.
#'  Default is 0.90 (90\% of the proteins).
#'
#' @return Returns one plot for each sample, being the dynamic range.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[["proteinGroups.txt"]]
#' PlotAllDynamicRange(proteinGroups)
#'
PlotAllDynamicRange <- function(proteinGroups,
                                show_shade = TRUE,
                                percent_proteins = 0.90){



  rank_groups<-  proteinGroups %>%  select(contains("Intensity ")) %>% select(-starts_with('LFQ'))

  rank_groups <- log10(rank_groups)

  pl <- vector("list", length = ncol(rank_groups))


  ##columns and rows depending of number of samples

  if(ncol(rank_groups)<=4){
    columns_grid <- 1
    rows_grid <- 4
  }

  if(ncol(rank_groups)>4){
    columns_grid <- 2
    rows_grid <- 4
  }



  for(i in 1:ncol(rank_groups)){

    temp <- data.frame(rank_groups[,i])

    colnames(temp) <- colnames(rank_groups)[i]

    assign(colnames(rank_groups)[i], temp)


    temp <- temp[order(temp[,1], decreasing=TRUE), ]

    temp <- temp[!grepl('^-Inf$', temp)]

    vec_temp <- seq(1:length(temp))

    temp_data <- data.frame(vec_temp, temp)




    temp_plot <- ggplot(temp_data, aes(x=vec_temp,y = temp))+
                      geom_point(colour='darkgrey', alpha=0.75, shape=21)+
                      ggtitle(colnames(rank_groups)[i])+
                      theme_bw()+
                      ylab(expression('log'[10]*'(Intensity)'))+
                      xlab('Protein Abundance Rank')


    if(show_shade ==TRUE){

      limits <- (1- percent_proteins)/2

      limits_row <- round(nrow(temp_data)*0.05)

      upper_y = temp_data$temp[limits_row]

      bottom_y = temp_data$temp[nrow(temp_data)-limits_row]

      orders_abundance_temp <- paste(round(upper_y-bottom_y,digits = 1), 'orders  of abundance')


        temp_plot <- temp_plot+
                          annotate('rect',
                                   xmin = limits_row,
                                   xmax = nrow(temp_data)-limits_row,
                                   ymin = bottom_y ,
                                   ymax = upper_y,
                                   alpha=0.3)+
                          annotate('text',
                                   x = nrow(temp_data)/2,
                                   y = bottom_y,
                                   label = orders_abundance_temp)+
                          annotate('text',
                                   x = nrow(temp_data)/2,
                                   y = upper_y,
                                   label = paste0(percent_proteins*100, ' % of proteins represented.'))

    }



    pl[[i]] <- temp_plot



    rm(temp)
    rm(vec_temp)
    rm(limits_row)
    rm(upper_y)
    rm(bottom_y)
    rm(orders_abundance_temp)

  }



  marrangeGrob(grobs=pl, ncol=columns_grid, nrow = rows_grid, top = NULL)



}




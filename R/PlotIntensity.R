#' Intensity / LFQ intensity per sample.
#'
#' @param proteinGroups  The proteinGroups.txt table from  MaxQuant Output.
#' @param split_violin_intensity If TRUE, both the LFQ and the Intensity will be
#'  shown in the same plot. If FALSE, it can be specified in the intensity_type
#'  which intensity to visualize.
#' @param intensity_type The type of intensity. Values: 'Intensity' or 'LFQ'.
#'  Only useful if split_violin_intensity = FALSE.  Default is Intensity.
#' @param  log_base The logarithmic scale for the intensity. Default is 2.
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @return A violin plot and boxplot of the intensities in each sample.
#' @export
#'
#' @examples
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' files <- ReadDataFromDir(MQPathCombined)
#' proteinGroups <- files[['proteinGroups.txt']]
#' PlotIntensity(proteinGroups)
#'
#'
PlotIntensity <- function(proteinGroups,
                          split_violin_intensity = TRUE,
                          intensity_type = 'Intensity',
                          log_base = 2,
                          long_names = FALSE,
                          sep_names = NULL,
                          palette = 'Set2'){

  id <- variable <- value <- x <- `violinwidth` <- xmin <- xmax <- xminv <- xmaxv <- y <- NULL

  ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
  }

  # Interleave (or zip) multiple units into one vector
  interleave <- function(...) UseMethod("interleave")

  interleave.unit <- function(...) {
    do.call("unit.c", do.call("interleave.default", lapply(list(...), as.list)))
  }

  interleave.default <- function(...) {
    vectors <- list(...)

    # Check lengths
    lengths <- unique(setdiff(vapply(vectors, length, integer(1)), 1L))
    if (length(lengths) == 0) lengths <- 1
    if (length(lengths) > 1) abort("`lengths` must be below 1")

    # Replicate elements of length one up to correct length
    singletons <- vapply(vectors, length, integer(1)) == 1L
    vectors[singletons] <- lapply(vectors[singletons], rep, lengths)

    # Interleave vectors
    n <- lengths
    p <- length(vectors)
    interleave <- rep(seq_len(n), each = p) + seq(0, p - 1) * n
    unlist(vectors, recursive = FALSE)[interleave]
  }

  create_quantile_segment_frame <- function(data, draw_quantiles, split = FALSE, grp = NULL) {
    dens <- cumsum(data$density) / sum(data$density)
    ecdf <- stats::approxfun(dens, data$y)
    ys <- ecdf(draw_quantiles)
    violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
    violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
    violin.xs <- (stats::approxfun(data$y, data$x))(ys)
    if (grp %% 2 == 0) {
      data.frame(
        x = interleave(violin.xs, violin.xmaxvs),
        y = rep(ys, each = 2), group = rep(ys, each = 2)
      )
    } else {
      data.frame(
        x = interleave(violin.xminvs, violin.xs),
        y = rep(ys, each = 2), group = rep(ys, each = 2)
      )
    }
  }

  GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                             draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- dplyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                 stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                           1))
                                 quantiles <- create_quantile_segment_frame(data, draw_quantiles)
                                 aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                 aesthetics$alpha <- rep(1, nrow(quantiles))
                                 both <- cbind(quantiles, aesthetics)
                                 quantile_grob <- GeomPath$draw_panel(both, ...)
                                 ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                 ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                             })

  geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                                draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
  }

  intensities <- proteinGroups %>%  select(id, contains('Intensity '))

  colourCount = length(colnames(intensities %>%  select(-contains(c("id",'LFQ')))))

  getPalette = colorRampPalette(brewer.pal(8, palette))


  if(split_violin_intensity == TRUE){

    #Error if no LFQ was found, plot intensities
    if(length(intensities %>% select(contains('LFQ')))== 0){

      #Print a warning that split violin will not be created and the intensities will be plotted.
      print('LFQ intensities not found, split_violin_plot can not be created')
      print('Changing intensity automatically to Intensity')

        intensities <-  intensities %>%  select(-contains('LFQ'))
        title <- 'Intensity'
        colnames(intensities)[-1] <- gsub('Intensity','',colnames(intensities)[-1])

        intensities_measure <- colnames(intensities)
        intensities_measure <- intensities_measure[! intensities_measure %in% 'id']

      if(log_base == 2){
        melted_intensities <- melt(log2(intensities), id.vars = 'id', measure.vars = intensities_measure)
        ylab <- expression('Log'[2]*'(Intensity)')
      }

      if (log_base == 10){
        melted_intensities <- melt(log10(intensities), id.vars = 'id', measure.vars = intensities_measure)
        ylab <- expression('Log'[10]*'(Intensity)')
      }
      b <-   ggplot(melted_intensities, aes(x = variable, y = value, color = variable))+
                geom_violin(fill = 'gray80', size = 1, alpha = .5)+
                geom_boxplot(width=0.2, outlier.shape = NA)+
                ggtitle(title)+
                xlab('Experiment')+
                ylab(ylab)+
                theme_bw()+
                theme(legend.position = 'none')+
                scale_color_manual(values = getPalette(colourCount))

      if(long_names==TRUE){
        b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
      } else{
        b
      }
    #Create the split_violin_plots
    } else{
          #create a table with the columns : id, sample, group, value
          df <-intensities %>%
                 pivot_longer(-id,
                         names_to = c("intensity_type", "sample" ),
                         names_prefix = 'LFQ intensity',
                         names_sep = ' ',
                         values_to = "value")

          df$intensity_type[df$intensity_type == ''] = 'LFQ intensity'

         if (log_base == 10) {

           df$value <- log10(df$value)
           ylab <- expression('Log'[10]*'(Intensity)')

         }

         if (log_base == 2) {

           df$value <- log2(df$value)
           ylab <- expression('Log'[2]*'(Intensity)')

         }

          a <- ggplot(df, aes(sample, value, fill = intensity_type))+
                    geom_split_violin()+
                    geom_boxplot(width=0.2, outlier.shape = NA)+
                    ggtitle('Protein Intensity & LFQ intensity')+
                    xlab('Experiment')+
                    ylab(ylab)+
                    theme_bw()+
                    scale_fill_brewer(palette = palette)+
                    theme(legend.position = 'bottom')

        if(long_names==TRUE){
          a + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
        } else{
          a
        }
    }
  #if split_violin_plot == FALSE, Intensity or LFQ intensity will be plotted.

  } else if  (split_violin_intensity == FALSE) {

      if (intensity_type == 'Intensity') {
        intensities <-  intensities %>%  select(-contains('LFQ'))
        title <- 'Intensity'
        colnames(intensities)[-1] <- gsub('Intensity','',colnames(intensities)[-1])
      }

      if (intensity_type == 'LFQ'){
        intensities <-  intensities %>%  select(id, contains('LFQ intensity '))
        title <- 'LFQ intensity'
        colnames(intensities)[-1] <- gsub('LFQ intensity','',colnames(intensities)[-1])

        #Error if LFQ Intensity not found.

        if (length(intensities) == 1) {
          print('LFQ intensities not found, changing automatically to Intensity.')

          intensities <-  proteinGroups %>%  select(id, contains('Intensity ')& -contains('LFQ'))
          title <- 'Intensity'
        }
      }

      intensities_measure <- colnames(intensities)
      intensities_measure <- intensities_measure[! intensities_measure %in% 'id']

      if(log_base == 2){
        melted_intensities <- melt(log2(intensities), id.vars = 'id', measure.vars = intensities_measure)
      }

      if (log_base == 10){
        melted_intensities <- melt(log10(intensities), id.vars = 'id', measure.vars = intensities_measure)
      }

      #For the y_lab

      if(intensity_type == 'Intensity' & log_base == 2){
        ylab <- expression('Log'[2]*'(Intensity)')
      }
      if(intensity_type == 'Intensity' & log_base == 10){
        ylab <- expression('Log'[10]*'(Intensity)')
      }
      if(intensity_type == 'LFQ' & log_base == 2){
        ylab <- expression('Log'[2]*'(LFQ intensity)')
      }
      if(intensity_type == 'LFQ' & log_base == 10){
        ylab <- expression('Log'[10]*'(LFQ intensity)')
      }

      b <-   ggplot(melted_intensities, aes(x = variable, y = value, color = variable))+
                geom_violin(fill = 'gray80', size = 1, alpha = .5)+
                geom_boxplot(width=0.2, outlier.shape = NA)+
                ggtitle(title)+
                xlab('Experiment')+
                ylab(ylab)+
                theme_bw()+
                theme(legend.position = 'none')+
                scale_color_manual(values = getPalette(colourCount))

    if(long_names==TRUE){
      b + scale_x_discrete(labels = function(x) stringr::str_wrap(gsub(sep_names,' ',x), 3))
    } else{
      b
    }

}


}

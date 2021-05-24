#' @import RColorBrewer
#' @import ggplot2
#' @import ggpubr
#' @import rmarkdown
#' @import ggridges
#' @import purrr
#' @importFrom stats aggregate complete.cases lm coef median
#' @importFrom readr read_delim
#' @importFrom magrittr %>%
#' @importFrom dplyr contains summarise select group_by filter n arrange
#' coalesce full_join mutate
#' @importFrom reshape2 melt dcast
#' @importFrom gridExtra marrangeGrob
#' @importFrom utils head
#' @importFrom stringr str_count str_wrap str_replace_all str_squish
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom tidyr pivot_longer starts_with separate_rows pivot_wider
#' @importFrom knitr kable
#' @importFrom scales zero_range
#' @importFrom grid grobName grobTree unit.c grid.draw
#' @importFrom rlang abort
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom gtable gtable_add_grob gtable_add_cols
#' @importFrom plyr defaults
#' @importFrom ggrepel geom_text_repel
NULL


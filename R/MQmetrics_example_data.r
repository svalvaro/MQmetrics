#' @import RColorBrewer
#' @import ggplot2
#' @import ggpubr
#' @import rmarkdown
#' @import ggridges
#' @importFrom stats aggregate complete.cases lm coef median
#' @importFrom readr read_delim
#' @importFrom magrittr %>%
#' @importFrom dplyr contains summarise select group_by filter n arrange coalesce full_join
#' @importFrom reshape2 melt dcast
#' @importFrom gridExtra marrangeGrob
#' @importFrom utils head
#' @importFrom stringr str_count str_wrap
#' @importFrom chron times
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom tidyr pivot_longer starts_with separate_rows
#' @importFrom knitr kable
#' @importFrom scales zero_range
#' @importFrom grid grobName grobTree
#' @importFrom rlang abort
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
NULL

#' Directory to the MaxQuant Output created automatically named combined.
#'
#' In this combined foulder should be at least two folders: txt and proc.
#'
#' @docType data
#' @keywords Value
#' @name MQPathCombined
#' @usage data("MQmetrics_example_data")
#' @format Value.
NULL



#' evidence.txt
#'
#' The evidence file combines all the information about the identified peptides
#' and normally is the only file  required for processing the results.
#'
#' @docType data
#' @keywords data.frame
#' @name evidence
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL


#' modificationSpecificPeptides.txt
#'
#'
#' @docType data
#' @keywords data.frame
#' @name modificationSpecificPeptides
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' msScans.txt
#'
#' The msScans table contains information about the full scans, which can be used to verify data quality and
#' generated useful statistics about the interaction between the samples and LC.
#'
#' @docType data
#' @keywords data.frame
#' @name msScans
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL


#' msmsScans.txt
#'
#'
#' @docType data
#' @keywords data.frame
#' @name msmsScans
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' parameters.txt
#'
#' @docType data
#' @keywords data.frame
#' @name parameters
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' peptides.txt
#'
#' @docType data
#' @keywords data.frame
#' @name peptides
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' proteinGroups.txt
#'
#' The Protein Groups table contains information on the identified proteins in the processed raw-files. Each
#' single row contains the group of proteins that could be reconstructed from a set of peptides.
#'
#' @docType data
#' @keywords data.frame
#' @name proteinGroups
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' summary.txt
#'
#' The summary file contains summary information for all the raw files processed with a single MaxQuant run.
#' The summary information consists of some MaxQuant parameters, information of the raw file contents, and
#' statistics on the peak detection. Based on this file a quick overview can be gathered on the quality of the data
#' in the raw file.
#' The last row in this file contains the summary information for each column on each of the processed files.
#'
#' @docType data
#' @keywords data.frame
#' @name summary
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL

#' #runningTimes.txt
#'
#'
#' @docType data
#' @keywords data.frame
#' @name runningTimes
#' @usage data("MQmetrics_example_data")
#' @format data.frame
NULL


#' Generates a report including all the plots of MQmetrics.
#'
#' @param MQPathCombined The directory to the "combined" folder where the
#'  MaxQuant results are stored.
#'
#' @param output_dir The directory where the results will be stored. By default
#'  is the working directory.
#'
#' @param remove_contaminants Whether or not to remove contaminants, reverse and identified by one one peptide.
#'
#' @param log_base The logarithmic scale for the intensity. Default is 2.
#'
#' @param long_names If TRUE, samples having long names will be considered, and
#'  the name will be split by sep_names. By default = FALSE.
#'
#' @param sep_names If long_names is TRUE, sep_names has to be selected. Samples
#'  names will be split. By default is NULL.
#'
#' @param intensity_type The type of intensity of interest. Values: 'Intensity'
#'  or 'LFQ'. Default = 'Intensity'.
#'
#' @param palette The palette from the Package RColorBrewer. By default is 'Set2'.
#'
#' @param UniprotID Uniprot ID of the protein of interest. \code{PlotProteinCoverage()}.
#'
#' @param segment_width Width of the segments to improve visualization. Default is 1.
#'  (PlotProteinCoverage).
#'
#' @param show_shade Creates a shade showing where the \code{percent_proteins} are.
#'  Default is TRUE. \code{PlotAllDynamicRange(), PlotCombinedDynamicRange()}.
#'
#' @param percent_proteins  Determines the percentage for the show_shade parameter.
#'  Default is 0.90 (90\% of the proteins). \code{PlotAllDynamicRange(), PlotCombinedDynamicRange()}.
#'
#' @param show_calibrated_rt  If TRUE, it will also show the calibrated retention
#'  time of each iRT peptide. By default = FALSE. \code{PlotiRT()}.
#'
#' @param tolerance Error maximum to find the iRT peptides by m/z value.
#'  by default is 0.001.
#' @param show_max_value If TRUE, it will show the max TIC value of each sample. \code{PlotTotalIonCurrent()}.
#'
#' @param peptides_modified Minimum number of peptides modified. Default  is 5. \code{PlotPTM()}.
#'
#' @param show_median If true it will show the median of each group, as a red
#'  dashed line.By default is TRUE. \code{PlotHydrophobicity()}.
#'
#' @param size_median The width of the median line in the plots.
#'
#' @param binwidth Selects the binwidth of the histogram. By default = 0.2. \code{PlotHydrophobicity()}.
#'
#' @param plot_unmodified_peptides If TRUE, it will show the Unmodified peptides.\code{PlotPTM()}.
#'
#'
#' @return A pdf document with all the results of MQmetrics package.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
#' generateReport(MQPathCombined)
#' }
generateReport = function(MQPathCombined,
                          output_dir = getwd(),
                          remove_contaminants = TRUE,
                          log_base = 2,
                          long_names = FALSE,
                          sep_names = NULL,
                          intensity_type = 'Intensity',
                          palette = 'Set2',
                          UniprotID = NULL,
                          segment_width = 1,
                          show_shade = TRUE,
                          percent_proteins = 0.90,
                          show_calibrated_rt = FALSE,
                          tolerance = 0.001,
                          show_max_value = TRUE,
                          peptides_modified = 1,
                          show_median = TRUE,
                          size_median = 1.5,
                          binwidth = 0.1,
                          plot_unmodified_peptides = FALSE,
                          plots_per_page = 5){




  #Determine the template
  input = system.file("rmd/template_report.Rmd", package="MQmetrics")



  rmarkdown::render(input = input,
                    params = list(input_dir = MQPathCombined,
                                  remove_contaminants = remove_contaminants,
                                  log_base = log_base,
                                  long_names = long_names,
                                  sep_names = sep_names,
                                  intensity_type = intensity_type,
                                  palette = palette,
                                  UniprotID = UniprotID,
                                  segment_width = segment_width,
                                  show_shade = show_shade,
                                  percent_proteins = percent_proteins,
                                  show_calibrated_rt = show_calibrated_rt,
                                  tolerance = tolerance,
                                  show_max_value = show_max_value,
                                  peptides_modified = peptides_modified,
                                  show_median = show_median,
                                  size_median = size_median,
                                  binwidth = binwidth,
                                  plot_unmodified_peptides = plot_unmodified_peptides,
                                  plots_per_page = plots_per_page),
                    output_file = "MQmetrics_report.pdf",
                    output_dir = output_dir,
                    clean = TRUE)


}

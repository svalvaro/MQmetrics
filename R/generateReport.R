#' Genearte a report with all functions combined
#'
#' @param input_dir
#' @param output_format
#' @param output_file
#' @param output_dir
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generateReport = function(MQPathCombined,
                          output_dir = getwd(),
                          report_tables = FALSE,
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
                          show_max_value = TRUE,
                          peptides_modified = 3,
                          show_median = TRUE,
                          binwidth = 0.1){




  #Determine the template
  input = system.file("rmd/template_report.Rmd", package="MQmetrics")



  rmarkdown::render(input = input,
                    params = list(input_dir = MQPathCombined,
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
                                  show_max_value = show_max_value,
                                  peptides_modified = peptides_modified,
                                  show_median = show_median,
                                  binwidth = binwidth),
                    output_file = "MQmetrics_report.pdf",
                    output_dir = output_dir,
                    clean = TRUE)


  #Determine the template for the report table,

  if(report_tables == TRUE){

    input2 = system.file("rmd/tables_template.Rmd", package = "MQmetrics")

    rmarkdown::render(input = input2,
                      params = list(input_dir = MQPathCombined,
                                    log_base = log_base,
                                    intensity_type = intensity_type),
                      output_file = "MQmetrics_report_tables.pdf",
                      output_dir = output_dir,
                      clean = TRUE)

  }

  # #Run the render
  # outputFileName  = do.call('render',args=args)
  # invisible(outputFileName)
}

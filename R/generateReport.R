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
generateReport = function(input_dir,
                          output_dir = getwd(),
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
                          peptides_modified = 3){




  #Determine the template
  input = system.file("rmd/template_report.Rmd", package="MQmetrics")


  rmarkdown::render(input = input,
                    params = list(input_dir=input_dir,
                                  log_base = log_base,
                                  long_names = long_names,
                                  sep_names = sep_names,
                                  intensity_type = intensity_type,
                                  palette = palette,
                                  UniprotID=UniprotID,
                                  segment_width = segment_width,
                                  show_shade = show_shade,
                                  percent_proteins = percent_proteins,
                                  show_calibrated_rt = show_calibrated_rt,
                                  show_max_value = show_max_value,
                                  peptides_modified = peptides_modified),
                    output_file = "report.pdf",
                    output_dir = output_dir,
                    clean = TRUE)
  # #Run the render
  # outputFileName  = do.call('render',args=args)
  # invisible(outputFileName)
}

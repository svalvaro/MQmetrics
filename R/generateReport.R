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
                          UniprotID = NULL,
                          long_names = FALSE,
                          sep_names = NULL){




  #Determine the template
  input = system.file("rmd/template_report.Rmd", package="MQmetrics")


  rmarkdown::render(input = input,
                    params = list(input_dir=input_dir,
                                  UniprotID=UniprotID,
                                  long_names = long_names,
                                  sep_names = sep_names),
                    output_file = "report.pdf",
                    output_dir = output_dir,
                    clean = TRUE)
  # #Run the render
  # outputFileName  = do.call('render',args=args)
  # invisible(outputFileName)
}

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
generateReport = function(input_dir, dir = getwd(), UniprotID=NULL){




  #Determine the template
  input = system.file("rmd/template_report.Rmd", package="MQmetrics")


  rmarkdown::render(input = input,
                    params = list(input_dir=input_dir,
                                  UniprotID=UniprotID),
                    output_file = "report.pdf",
                    output_dir = dir,
                    clean = TRUE)
  # #Run the render
  # outputFileName  = do.call('render',args=args)
  # invisible(outputFileName)
}

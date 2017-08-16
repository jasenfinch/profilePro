#' profileProcess
#' @description process metabolomic profiling data
#' @param files character vector of file paths to use for processing
#' @param info file path for info file
#' @param parameters object of class ProfileParameters containing the parameters for processing
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

profileProcess <- function(files,info,parameters) {
  
  info <- suppressMessages(read_csv(info))
  
  x <- new('MetaboProfile',
           log = date(),
           files = files,
           processingParameters = parameters,
           Info = info,
           Data = tibble(),
           processingResults = list()
           )
  
  method <- profilingMethods(parameters@technique)
  
  x <- method(x)
  
  return(x)
}
#' profileProcess
#' @description process metabolomic profiling data
#' @param files character vector of file paths to use for processing along with the info file path
#' @param parameters object of class ProfileParameters containing the parameters for processing
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

profileProcess <- function(files,parameters) {
  
  if (!(T %in% grepl(parameters@infoName,files))) {
    stop('No sample info found!')
  }
  
  info <- suppressMessages(read_csv(files[grep(parameters@infoName,files)]))
  files <- files[-grep(parameters@infoName,files)]
  
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
#' profileProcess
#' @description process metabolomic profiling data
#' @param files character vector of file paths to use for processing
#' @param info tibble containing sample info
#' @param parameters object of class ProfileParameters containing the parameters for processing
#' @examples 
#' \dontrun{
#' library(stringr)
#' library(faahKO)
#' library(tibble)
#' library(readr)
#'
#' files <- list.files(system.file("cdf", package = "faahKO"),full.names = TRUE,recursive = T)
#'
#' # make runinfo file
#' filesSplit <- str_split_fixed(files,'/',str_count(files[1],'/') + 1) 
#' info <- tibble(fileOrder = 1:length(files),
#'               injOrder = 1:length(files),
#'               fileName = filesSplit[,ncol(filesSplit)], 
#'               batch = rep(1,length(files)), 
#'               batchBlock = rep(1,length(files)), 
#'               name = str_replace_all(filesSplit[,ncol(filesSplit)],'.CDF',''), 
#'               class = filesSplit[,ncol(filesSplit)-1])
#'
#' files <- list(pos = files)
#' 
#' # prepare parameters
#' parameters <- profileParameters('LCMS-RP')
#' parameters@processingParameters$peakDetection <- CentWaveParam(snthresh = 20, noise = 1000)
#' parameters@processingParameters$retentionTimeCorrection <- ObiwarpParam()
#' parameters@processingParameters$grouping <- PeakDensityParam(sampleGroups = info$class,
#'                                                              maxFeatures = 300,
#'                                                              minFraction = 2/3)
#' # run processing
#' processedData <- profileProcess(files,info,parameters)
#'
#' }
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

profileProcess <- function(files,info,parameters) {
  
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

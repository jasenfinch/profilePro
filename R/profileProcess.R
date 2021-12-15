#' profileProcess
#' @description process metabolomic profiling data
#' @param file_paths character vector of file paths to use for processing
#' @param sample_info tibble containing sample info
#' @param parameters object of class ProfileParameters containing the parameters for processing
#' @return An S4 object of class MetaboProfile
#' @seealso \code{\link{profileParameters}}
#' @examples 
#' \dontrun{
#' # LCMS-RP example using the faahKO package data
#' ## Retrieve file paths
#' file_paths <- list.files(
#'   system.file("cdf", 
#'             package = "faahKO"),
#'   full.names = TRUE,
#'   recursive = TRUE)[1:2]
#'   file_names <- basename(file_paths)
#' sample_names <- tools::file_path_sans_ext(file_names)
#' 
#' ## Generate sample information table
#' sample_info <- tibble(fileOrder = seq_along(file_paths),
#'                       injOrder = seq_along(file_paths),
#'                       fileName = file_names,
#'                       batch = 1,
#'                       block = 1,
#'                       name = sample_names,
#'                       class = substr(sample_names,1,2))
#' 
#' ## Generate profiling parameters
#' parameters <- profileParameters('LCMS-RP')
#' processingParameters(parameters)$peakDetection <- CentWaveParam(snthresh = 20, 
#'                                                                 noise = 1000)
#' processingParameters(parameters)$retentionTimeCorrection <- ObiwarpParam()
#' processingParameters(parameters)$grouping <- PeakDensityParam(sampleGroups = sample_info$class,
#'                                                               maxFeatures = 300,
#'                                                               minFraction = 2/3)
#' ## Specify parallel processing plan
#' plan('sequential')
#' 
#' ## Process data
#' processed_data <- profileProcess(file_paths,sample_info,parameters)
#' 
#' # GCMS-XCMS example using the gcspikelite package data
#' ## Retrieve file paths
#' file_paths <- list.files(
#'   system.file('data',
#'             package = 'gcspikelite'),
#'   pattern = '.CDF',
#'   full.names = TRUE)[1:2]
#' file_names <- basename(file_paths)
#' sample_names <- tools::file_path_sans_ext(file_names)
#' 
#' ## Generate sample information table
#' sample_info <- tibble(fileOrder = seq_along(file_paths),
#'                       injOrder = seq_along(file_paths),
#'                       fileName = file_names,
#'                       batch = 1,
#'                       block = 1,
#'                       name = sample_names,
#'                       class = targets$Group[1:2])
#' 
#' ## Generate profiling parameters
#' parameters <- profileParameters('GCMS-XCMS')
#' 
#' ## Specify parallel processing plan
#' plan('sequential')
#' 
#' ## Process data
#' processed_data <- profileProcess(file_paths,sample_info,parameters)
#' 
#' # GCMS-eRah example using the gcspikelite package data
#' ## Retrieve file paths
#' file_paths <- list.files(
#'   system.file('data',
#'             package = 'gcspikelite'),
#'   pattern = '.CDF',
#'   full.names = TRUE)[1:2]
#' file_names <- basename(file_paths)
#' sample_names <- tools::file_path_sans_ext(file_names)
#' 
#' ## Generate sample information table
#' sample_info <- tibble(fileOrder = seq_along(file_paths),
#'                       injOrder = seq_along(file_paths),
#'                       fileName = file_names,
#'                       batch = 1,
#'                       block = 1,
#'                       name = sample_names,
#'                       class = targets$Group[1:2])
#' 
#' ## Generate profiling parameters
#' parameters <- profileParameters('GCMS-eRah')
#' 
#' ## Specify parallel processing plan
#' plan('sequential')
#' 
#' ## Process data
#' processed_data <- profileProcess(file_paths,sample_info,parameters)
#' }
#' @importFrom tibble tibble
#' @importFrom methods new
#' @importFrom dplyr arrange
#' @export

profileProcess <- function(file_paths,sample_info,parameters) {
  
  file_paths <- file_paths[order(sample_info$injOrder)]
  sample_info <- sample_info %>%
    arrange(injOrder)
  
  x <- new('MetaboProfile',parameters,
           file_paths = file_paths,
           sample_info = sample_info)
  
  method <- parameters %>%
    technique() %>%
    profilingMethods()
  
  x <- method(x)
  
  return(x)
}


profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = erahProcessing,
    
    `GCMS-XCMS` = GCMSprocessing,
    
    `LCMS-RP` = XCMSprocessing,
    
    `LCMS-NP` = XCMSprocessing
  )
  
  method <- methods[[method]]
  
  return(method)
}

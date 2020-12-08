#' profileProcess
#' @description process metabolomic profiling data
#' @param file_paths character vector of file paths to use for processing
#' @param sample_info tibble containing sample info
#' @param parameters object of class ProfileParameters containing the parameters for processing
#' @examples 
#' \dontrun{
#' # Get sample information and use only the three injection for this example
#' info <- metaboData::runinfo('RP-UHPLC-HRMS','BdistachyonEcotypes') %>%
#'   dplyr::filter(injOrder %in% seq_len(3))
#' 
#' # Retrieve file paths
#' files <- metaboData::filePaths('RP-UHPLC-HRMS','BdistachyonEcotypes') %>%
#'   {
#'     .[base::basename(.) %in% info$fileName] 
#'   }
#' 
#' # Prepare parameters
#' parameters <- profileParameters('LCMS-RP')
#' 
#' # Run processing
#' pd <- profileProcess(files,info,parameters)
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

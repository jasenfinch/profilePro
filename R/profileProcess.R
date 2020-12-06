#' profileProcess
#' @description process metabolomic profiling data
#' @param files character vector of file paths to use for processing
#' @param info tibble containing sample info
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

profileProcess <- function(files,info,parameters) {
  
  files <- files[order(info$injOrder)]
  info <- info %>%
    arrange(injOrder)
  
  x <- new('MetaboProfile',parameters)
  files(x) <- files
  sampleInfo(x) <- info
  
  method <- parameters %>%
    technique() %>%
    profilingMethods()
  
  x <- method(x)
  
  return(x)
}

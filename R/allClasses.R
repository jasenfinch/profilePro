#' ProfileParameters
#' @description An S4 class to store profile processing parameters.
#' @slot technique the profiling technique to use. Defaults to the result of \code{availableTechniques()[1]}
#' @slot  processing_parameters a list containing the parameters to use for processing
#' @export

setClass('ProfileParameters',
         slots = list(
           technique = 'character',
           processing_parameters = 'list'
         ),
         prototype = list(
           technique = availableTechinques()[1]
         )
)

setValidity('ProfileParameters',function(object){
  if (!(object@technique %in% availableTechinques())) {
    str_c('Technique should be one of ',
          str_c(availableTechinques(),collapse = ', '),
          '.')
  } else {
    return(TRUE)
}
})

#' MetaboProfile
#' @description An S4 class to store the profile processing results
#' @slot version package version
#' @slot creation_date date and time of the initiation of processing
#' @slot file_paths list of file paths to used for processing. Vectors of files form different aquisition modes should be labelled accordingly. 
#' @slot processing_parameters object of class ProfileParameters containing the parameters for processing
#' @slot sample_information tibble containing runinfo data
#' @slot data list containing tibbles of processed data
#' @slot processing_results list containing processing results
#' @importFrom utils packageVersion
#' @export

setClass('MetaboProfile',
         slots = list(
           version = 'character',
           creation_date = 'character',
           file_paths = 'character',
           sample_info = 'tbl_df',
           data = 'list',
           processing_results = 'list'
         ),
         contains = 'ProfileParameters',
         prototype = list(
           version = packageVersion('profilePro') %>%
             as.character(),
           creation_date = date(),
           sample_info = tibble(
             fileOrder = character(),
             injOrder = numeric(),
             fileName = character(),
             batch = numeric(),
             block = numeric(),
             name = character(),
             class = character()
           )
         )
)

setValidity('MetaboProfile',function(object){
  necessary_names <- c('fileOrder','injOrder','fileName','batch','block','name','class')
  
  info_names <- object %>%
    sampleInfo() %>%
    colnames()
  
  presence <- necessary_names %in% info_names
  
  if (FALSE %in% presence) {
    str_c('Sample information should contain the following column names: ',
          str_c(necessary_names,collapse = ', '),
          '.')
  } else {
    TRUE
  }
})

setValidity('MetaboProfile',function(object){
  file_path_names <- object %>%
    filePaths() %>%
    basename()
  
  info_file_names <- object %>%
    sampleInfo() %>%
    .$fileName
  
  matching <- file_path_names == info_file_names
  
  if (FALSE %in% matching) {
    'File names in paths do not match file names in the sample information.'
  } else {
    TRUE
  }
  
  
})
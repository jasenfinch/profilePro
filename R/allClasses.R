#' ProfileParameters
#' @description An S4 class to store profile processing parameters.
#' @slot technique the profiling technique to use
#' @slot  processing_parameters a list containing the parameters to use for processing
#' @export

setClass('ProfileParameters',
         slots = list(
           technique = 'character',
           processing_parameters = 'list'
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
#' @slot files list of file paths to used for processing. Vectors of files form different aquisition modes should be labelled accordingly. 
#' @slot processing_parameters object of class ProfileParameters containing the parameters for processing
#' @slot Info tibble containing runinfo data
#' @slot Data list containing tibbles of processed data
#' @slot processingResults list containing processing results
#' @importFrom utils packageVersion
#' @export

setClass('MetaboProfile',
         slots = list(
           version = 'character',
           creation_date = 'character',
           files = 'character',
           Info = 'tbl_df',
           Data = 'list',
           processing_results = 'list'
         ),
         contains = 'ProfileParameters',
         prototype = list(
           version = packageVersion('profilePro') %>%
             as.character(),
           creation_date = date()
         )
)
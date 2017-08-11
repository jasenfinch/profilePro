#' ProfileParameters
#' @description An S4 class to store profile processing parameters.
#' @slot technique the profiling technique to use
#' @slot infoName name of file containing run information
#' @slot  processingParameters a list containing the parameters to use for processing
#' @export

setClass('ProfileParameters',
         slots = list(
           technique = 'character',
           infoName = 'character',
           processingParameters = 'list'
         )
)

#' MetaboProfile
#' @description An S4 class to store the profile processing results
#' @slot log date and time of the initiation of processing
#' @slot files character vector of file paths to used for processing
#' @slot processingParameters object of class ProfileParameters containing the parameters for processing
#' @slot Info tibble containing runinfo data
#' @slot Data list containing tibbles of processed data
#' @slot processingResults list containing processing results
#' @export

setClass('MetaboProfile',
         slots = list(
           log = 'character',
           files = 'character',
           processingParameters = 'ProfileParameters',
           Info = 'tbl_df',
           Data = 'list',
           processingResults = 'list'
         )
)
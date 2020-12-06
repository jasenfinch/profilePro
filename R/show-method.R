#' show-ProfileParameters
#' @description show method for ProfileParameters class
#' @param object S4 object of class ProfileParameters
#' @importFrom methods show
#' @export

setMethod('show',signature = 'ProfileParameters',
          function(object){
            cat('Processing parameters for technique',object@technique)
          }
)

#' show-MetaboProfile
#' @description show method for MetaboProfile class
#' @param object S4 object of class MetaboProfile
#' @importFrom purrr map_dbl
#' @importFrom crayon bold red blue
#' @export

setMethod('show',signature = 'MetaboProfile',
          function(object){
            cat('\nprofilePro MetaboProfile\n')
            cat('Analysed by package version',bold(red(object@log$version[1])))
            cat('\n',object@log$date,'\n',sep = '')
            cat('Technique:',bold(blue(object@processingParameters@technique)),'\n')
            cat('No. Samples:',sum(map_dbl(object@files,length)), '\n\n')
          }
)

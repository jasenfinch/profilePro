#' show-ProfileParameters
#' @description show method for ProfileParameters class
#' @param object S4 object of class ProfileParameters
#' @importFrom methods show
#' @export

setMethod('show',signature = 'ProfileParameters',
          function(object){
            cat('Processing parameters for technique',
                object %>% 
                  technique() %>% 
                  blue(),
                '\n\n')
            
            object %>%  processingParameters()
            
            out <- params %>% 
              names() %>% 
              map(~{
                cat(blue(.x),':\n',sep = '')
                
                current <- params[[.x]] 
                
                if (is.list(current)){
                  current <- unlist(current)
                }
                
                if (isS4(current)) {
                  print(current)
                } else {
                  cat(current,'\n')  
                }
                cat('\n')
              })
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
            cat('\n',blue('profilePro'),' MetaboProfile\n',sep = '')
            cat('Analysed by package version',bold(red(version(object))))
            cat('\n',creationDate(object),'\n',sep = '')
            cat('Technique:',bold(blue(technique(object))),'\n')
            cat('No. Samples:',length(filePaths(object)), '\n\n')
          }
)

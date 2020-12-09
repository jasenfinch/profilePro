#' ProfileParameters get and set methods
#' @rdname parameters
#' @description Get and set profile processing parameters
#' @param x S4 object of class ProfileParameters
#' @param value value to set
#' @export

setMethod('technique',signature = 'ProfileParameters',
          function(x){
            x@technique
          })

#' @rdname parameters
#' @export

setMethod('technique<-',signature = 'ProfileParameters',
          function(x,value){
            x <- profileParameters(value)
            validObject(x)
            return(x)
          })

#' @rdname parameters
#' @export

setMethod('processingParameters',signature = 'ProfileParameters',
          function(x){
            x@processing_parameters
          })

#' @rdname parameters
#' @export

setMethod('processingParameters<-',signature = 'ProfileParameters',
          function(x,value){
            x@processing_parameters <- value
            validObject(x)
            return(x)
          })

#' MetaboProfile class get and set methods.
#' @rdname processed
#' @description Retrieve or set information for a MetaboProfile object.
#' @param x S4 object of class MetaboProfile
#' @param value value to set
#' @export

setMethod('version',signature = 'MetaboProfile',
          function(x){
            x@version 
          })

#' @rdname processed
#' @export

setMethod('creationDate',signature = 'MetaboProfile',
          function(x){
            x@creation_date
          })

#' @rdname processed
#' @export

setMethod('filePaths',signature = 'MetaboProfile',
          function(x){
            x@file_paths
          })

#' @rdname processed
#' @export

setMethod('sampleInfo',signature = 'MetaboProfile',
          function(x){
            x@sample_info
          }
)

#' @rdname processed
#' @export

setMethod('processingResults',signature = 'MetaboProfile',
          function(x){
            x@processing_results
          }
)

#' @rdname processed

setMethod('processingResults<-',signature = 'MetaboProfile',
          function(x,value){
            x@processing_results <- value
            validObject(x)
            return(x)
          }
)

#' @rdname processed
#' @export

setMethod('processedData',signature = 'MetaboProfile',
          function(x){
            x@data
          }
)

#' @rdname processed

setMethod('processedData<-',signature = 'MetaboProfile',
          function(x,value){
            x@data <- value
            return(x)
          }
)

#' @rdname processed
#' @export

setMethod('extractProcObject',signature = 'MetaboProfile',
          function(x){
            processingResults(x)$processed
          }
)

#' @rdname processed
#' @export

setMethod('peakInfo',signature = 'MetaboProfile',
          function(x){
            
            if (technique(x) == 'GCMS-eRah') {
              proc_object <- x %>%
                extractProcObject() %>%
                idList(id.database = processingParameters(x)$identification$compound_database)
            } else {
              processingResults(x)$peak_info
            }
          }
)
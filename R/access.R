#' ProfileParameters getters and setters
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
            x@technique <- value
            return(x)
          })

#' @rdname parameters
#' @export

setMethod('processingParameters',signature = 'ProfileParameters',
          function(x){
            x@processingParameters
          })

#' @rdname parameters
#' @export

setMethod('processingParameters<-',signature = 'ProfileParameters',
          function(x,value){
            x@processingParameters <- value
            return(x)
          })

#' MetaboProfile class get and set methods.
#' @rdname processed
#' @description Retrieve or set information for a MetaboProfile object.
#' @param x S4 object of class MetaboProfile
#' @param value value to set
#' @export

setMethod('sampleInfo',signature = 'MetaboProfile',
          function(x){
            x@Info
          }
)

#' @rdname processed
#' @export

setMethod('sampleInfo<-',signature = 'MetaboProfile',
          function(x,value){
            x@Info <- value
            return(x)
          }
)

#' @rdname processed
#' @export

setMethod('processedData',signature = 'MetaboProfile',
          function(x){
            x@Data
          }
)

#' @rdname processed
#' @export

setMethod('processedData<-',signature = 'MetaboProfile',
          function(x,value){
            x@Data <- value
            return(x)
          }
)

#' @rdname processed
#' @export

setMethod('extractProcObject',signature = 'MetaboProfile',
          function(x){
            x@processingResults$processed
          }
)

#' @rdname processed
#' @export

setMethod('peakInfo',signature = 'MetaboProfile',
          function(x){
            
            if (technique(x) == 'GCMS-eRah') {
              x %>%
                extractProcObject() %>%
                idList(
                  id.database = importGMD(filename = processingParameters(x)@processingParameters$identification$path, 
                                          DB.name = processingParameters(x)@processingParameters$identification$DBname, 
                                          DB.version = processingParameters(x)@processingParameters$identification$DBversion, 
                                          DB.info = processingParameters(x)@processingParameters$identification$DBinfo, 
                                          type = processingParameters(x)@processingParameters$identification$type)
                )
            } else {
              map(x@processingResults$peakInfo,~{
                .$definitions
              }) 
            }
          }
)
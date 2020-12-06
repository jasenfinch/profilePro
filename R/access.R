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

#' sampleInfo
#' @description Extract sample info from an object of class MetaboProfile.
#' @param x S4 object of class MetaboProfile
#' @rdname sampleInfo

setMethod('sampleInfo',signature = 'MetaboProfile',
          function(x){
            x@Info
          }
)

#' processedData
#' @description Extract processed data from an object of class MetaboProfile
#' @param x S4 object of class MetaboProfile
#' @rdname processedData

setMethod('processedData',signature = 'MetaboProfile',
          function(x){
            x@Data
          }
)

#' extractProcObject
#' @description Extract processing package object from an object of class MetaboProfile
#' @param x S4 object of class MetaboProfile
#' @rdname extractProcObject

setMethod('extractProcObject',signature = 'MetaboProfile',
          function(x){
            x@processingResults$processed
          }
)

#' peakInfo
#' @description Extract peak information from an object of class MetaboProfile
#' @param x S4 object of class MetaboProfile
#' @rdname peakInfo

setMethod('peakInfo',signature = 'MetaboProfile',
          function(x){
            
            if (str_detect(x@processingParameters@technique,'eRah')) {
              x@processingResults$processed %>%
                idList(
                  id.database = importGMD(filename = x@processingParameters@processingParameters$identification$path, 
                                          DB.name = x@processingParameters@processingParameters$identification$DBname, 
                                          DB.version = x@processingParameters@processingParameters$identification$DBversion, 
                                          DB.info = x@processingParameters@processingParameters$identification$DBinfo, 
                                          type = x@processingParameters@processingParameters$identification$type)
                )
            } else {
              map(x@processingResults$peakInfo,~{
                .$definitions
              }) 
            }
          }
)
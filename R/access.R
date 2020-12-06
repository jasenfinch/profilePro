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

#' Get or set processing parameters
#' @rdname processingParameters
#' @description Get or set processing parameters for an S4 object of class MetaboProfile.
#' @param x S4 object of class MetaboProfile
#' @param value list object of parameters to set
#' @return A list containing processing parameter objects.
#' @export

setMethod('processingParameters',signature = 'MetaboProfile',
          function(x){
            x@processingParameters
          })

#' @rdname processingParameters
#' @export

setMethod('processingParameters<-',signature = 'MetaboProfile',
          function(x,value){
            x@processingParameters <- value
            return(x)
          })

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
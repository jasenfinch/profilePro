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
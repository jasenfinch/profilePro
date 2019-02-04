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

#' extractXCMSnExp
#' @description Extract XCMSnExp objects from an object of class MetaboProfile
#' @param x S4 object of class MetaboProfile
#' @rdname extractXCMSnExp

setMethod('extractXCMSnExp',signature = 'MetaboProfile',
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
            map(x@processingResults$peakInfo,~{
              .$definitions
            })
          }
)
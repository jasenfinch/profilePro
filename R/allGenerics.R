
setGeneric("XCMSprocessing", function(x) {
  standardGeneric('XCMSprocessing')
})

setGeneric("erahProcessing", function(x) {
  standardGeneric('erahProcessing')
})


#' @rdname sampleInfo
#' @export

setGeneric('sampleInfo',function(x){
  standardGeneric('sampleInfo')
})

#' @rdname processedData
#' @export

setGeneric('processedData',function(x){
  standardGeneric('processedData')
})

#' @rdname extractProcObject
#' @export

setGeneric('extractProcObject',function(x){
  standardGeneric('extractProcObject')
})

#' @rdname peakInfo
#' @export

setGeneric('peakInfo',function(x){
  standardGeneric('peakInfo')
})

#' @rdname plotChromatogram
#' @export

setGeneric('plotChromatogram',function(processed, cls = NULL, group = F, alpha = 1, aggregationFun = 'max',...){
  standardGeneric('plotChromatogram')
})

#' @rdname plotTIC
#' @export

setGeneric('plotTIC',function(processed,by = 'injOrder', colour = 'block'){
  standardGeneric('plotTIC')
})



setGeneric("XCMSprocessing", function(x) 
  standardGeneric('XCMSprocessing'))

setGeneric("GCMSprocessing", function(x) 
  standardGeneric('GCMSprocessing'))

setGeneric("erahProcessing", function(x) 
  standardGeneric('erahProcessing'))

#' @rdname parameters

setGeneric('technique',function(x)
  standardGeneric('technique'))

#' @rdname parameters

setGeneric('technique<-',function(x,value) 
  standardGeneric('technique<-'))

#' @rdname parameters

setGeneric('processingParameters',function(x) 
  standardGeneric('processingParameters'))

#' @rdname parameters

setGeneric('processingParameters<-',function(x,value) 
  standardGeneric('processingParameters<-'))

#' @rdname processed

setGeneric('version',function(x) 
  standardGeneric('version'))

#' @rdname processed

setGeneric('creationDate',function(x) 
  standardGeneric('creationDate'))

#' @rdname processed

setGeneric('sampleInfo',function(x) 
  standardGeneric('sampleInfo'))

#' @rdname processed

setGeneric('sampleInfo<-',function(x,value) 
  standardGeneric('sampleInfo<-'))

#' @rdname processed

setGeneric('processedData',function(x) 
  standardGeneric('processedData'))

#' @rdname processed

setGeneric('processedData<-',function(x,value) 
  standardGeneric('processedData<-'))

#' @rdname processed
#' @export

setGeneric('extractProcObject',function(x) 
  standardGeneric('extractProcObject'))

#' @rdname processed
#' @export

setGeneric('peakInfo',function(x) 
  standardGeneric('peakInfo'))

#' @rdname plotChromatogram
#' @export

setGeneric('plotChromatogram',
           function(processed, 
                    cls = NULL, 
                    group = FALSE, 
                    alpha = 1, 
                    aggregationFun = 'max',...) 
             standardGeneric('plotChromatogram'))

#' @rdname plotTIC
#' @export

setGeneric('plotTIC',
           function(processed,
                    by = 'injOrder', 
                    colour = 'block') 
             standardGeneric('plotTIC'))


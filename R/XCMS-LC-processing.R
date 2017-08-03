#' @importFrom purrr map
#' @importFrom xcms xcmsSet retcor
#' @importFrom BiocParallel bpparam
#' @importFrom utils capture.output

XCMSlcProcessing <- function(x){
  parameters <- x@processingParameters
  modes <- parameters@processingParameters$modes
  
  peakDetection <- map(modes, ~{
    files <- x@files[grepl(.,x@files)]
    peakDet <- xcmsSet
    parameters@processingParameters$peakDetection$snames <- unlist(info[,parameters@processingParameters$peakDetection$snames])
    parameters@processingParameters$peakDetection$sclass <- unlist(info[,parameters@processingParameters$peakDetection$sclass])
    f <- formals(peakDet)
    f[names(parameters@processingParameters$peakDetection)] <- parameters@processingParameters$peakDetection
    formals(peakDet) <- f
    para <- bpparam()
    para@.xData$workers <- parameters@processingParameters$nCores
    formals(peakDet)$BPPARAM <- para
    x <- peakDet(files = files)
    return(x)
  }) 
  names(peakDetection) <- modes
  
  capture.output(retentionTimeCorrection <- map(peakDetection,retcor, method = parameters@processingParameters$retentionTimeCorrection$method, profStep = parameters@processingParameters$retentionTimeCorrection$profStep))
  
  x@Data <- tibble()
  x@processingResults <- list(peakDetection = peakDetection, retentionTimeCorrection = retentionTimeCorrection)
  return(x)
}

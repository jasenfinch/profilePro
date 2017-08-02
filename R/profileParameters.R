#' profileParameters
#' @importFrom parallel detectCores
#' @export

profileParameters <- function(technique) {
  parameters <- new('ProfileParameters',
                    technique = technique,
                    processingParameters = list()
                    )
  
  if (technique == 'GCMS-eRah') {
    
  }
  
  if (technique == 'GCMS-XCMS') {
    parameters@processingParameters <- list(
     peakDetection = list(method = 'matchedFilter',fwhm = 3, snthresh = 1,sclass = 'class',nSlaves = detectCores()),
     grouping = list(bw = 5,minfrac = 2/3)
    )
  }
  
  if (technique == 'LCMS-C18' | technique == 'LCMS-HILIC') {
    parameters@processingParameters <- list(
      peakDetection = list(method.pd = "centWave", ppm = 1.5, peakwidth = c(2,40),
                    snthresh = 3.0, mzCenterFun = "apex", mzdiff = 0.05, fitgauss = FALSE,
                    integrate = 2, nSlaves = detectCores()), 
      retentionTimeCorrection = list(method.rt = "obiwarp", profStep = 1.0),
      grouping = list(bw = 5 ,mzwid = 0.015, minfrac = 2/3)
      )
  }
  
  return(parameters)
}
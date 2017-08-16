#' profileParameters
#' @description Initiate default processing parameters for a given profiling technique.
#' @param technique the profiling technique for which to initiate parameters
#' @importFrom parallel detectCores
#' @importFrom xcms CentWaveParam ObiwarpParam PeakDensityParam MatchedFilterParam
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
      info = list(names = 'names', cls = 'class'),
      peakDetection = MatchedFilterParam(
        fwhm = 3, 
        snthresh = 1
      ),
      grouping = PeakDensityParam(
        bw = 5,
        minFraction = 0
      ),
      retentionTimeCorrection = PeakGroupsParam(
        smooth = 'loess'
      ),
      nCores = detectCores()
    )
  }
  
  if (technique == 'LCMS-RP' | technique == 'LCMS-NP') {
    parameters@processingParameters <- list(
      info = list(names = 'names', cls = 'class'),
      peakDetection = CentWaveParam(
        ppm = 1.5,
        peakwidth = c(2,40),
        snthresh = 3.0,
        mzCenterFun = "apex", 
        mzdiff = 0.05, 
        fitgauss = FALSE,
        integrate = 2
      ), 
      retentionTimeCorrection = ObiwarpParam(),
      grouping = PeakDensityParam(
        sampleGroups = 'class',
        bw = 5,
        binSize = 0.015, 
        minFraction = 0
      ),
    nCores = detectCores()
    )
  }
  
  return(parameters)
}
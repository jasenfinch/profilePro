#' Profiling paramters
#' @description Initiate default processing parameters for a given profiling technique.
#' @param technique the profiling technique for which to initiate parameters
#' @examples 
#' profileParameters('LCMS-RP')
#' @return An S4 object of class ProfileParameters.
#' @seealso \code{\link{availableTechniques}}, \code{\link{ProfileParameters-class}}, \code{\link{profileProcess}}
#' @importFrom xcms CentWaveParam ObiwarpParam PeakDensityParam MatchedFilterParam PeakGroupsParam FillChromPeaksParam
#' @importFrom erah setDecPar setAlPar
#' @export

profileParameters <- function(technique) {
  
  parameters <- new('ProfileParameters',
                    technique = technique)
  
  processingParameters(parameters) <- switch(
    technique,
    `GCMS-eRah` = erahParameters(),
    `GCMS-XCMS` = xcmsGCparameters(),
    `LCMS-RP` = xcmsRPparameters(),
    `LCMS-NP` = xcmsNPparameters()
    )
  
  return(parameters)
}

xcmsRPparameters <- function(){
  list(
    info = list(names = 'name', cls = 'class'),
    peakDetection = CentWaveParam(
      ppm = 8.2,
      peakwidth = c(16,43),
      snthresh = 7,
      mzCenterFun = "apex", 
      mzdiff = 0.001, 
      fitgauss = FALSE,
      integrate = 2
    ), 
    retentionTimeCorrection = ObiwarpParam(
      gapInit = 2.9,
      gapExtend = 2.7,
      binSize = 0.7
    ),
    grouping = PeakDensityParam(
      sampleGroups = 'class',
      bw = 1,
      binSize = 0.00775, 
      minFraction = 2/3
    ),
    infilling = FillChromPeaksParam()
  )
} 

xcmsNPparameters <- function(){
  list(
    info = list(names = 'name', cls = 'class'),
    peakDetection = CentWaveParam(
      ppm = 7.9,
      peakwidth = c(27,63),
      snthresh = 7.6,
      mzCenterFun = "apex", 
      mzdiff = 0.001, 
      fitgauss = FALSE,
      integrate = 2
    ), 
    retentionTimeCorrection = ObiwarpParam(
      gapInit = 2.9,
      gapExtend = 2.7,
      binSize = 1
    ),
    grouping = PeakDensityParam(
      sampleGroups = 'class',
      bw = 1,
      binSize = 0.00685, 
      minFraction = 2/3
    ),
    infilling = FillChromPeaksParam()
  )
} 

xcmsGCparameters <- function(){
  list(
    info = list(names = 'name', cls = 'class'),
    peakDetection = MatchedFilterParam(
      fwhm = 3, 
      snthresh = 1,
      max = 50
    ),
    retentionTimeCorrection = ObiwarpParam(),
    grouping = PeakDensityParam(
      sampleGroups = 'class',
      bw = 3,
      minFraction = 0,
      maxFeatures = 62
    ),
    infilling = FillChromPeaksParam()
  )
} 

erahParameters <- function(){
  
  cr <- formals(recMissComp)[-1]
  cr$min.samples <- 1
  
  list(
    cls = 'class',
    info = list(cls = 'class'),
    deconvolution = setDecPar(
      min.peak.width = 1.2, 
      avoid.processing.mz = c(73:75,
                              147:149)),
    alignment = setAlPar(min.spectra.cor = 0.90, 
                         max.time.dist = 3, 
                         mz.range = 70:600),
    compoundRecovery = cr,
    identification = list(compound_database = erah::mslib)
  )
}
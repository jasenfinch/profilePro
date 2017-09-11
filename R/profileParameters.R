#' profileParameters
#' @description Initiate default processing parameters for a given profiling technique.
#' @param technique the profiling technique for which to initiate parameters
#' @importFrom parallel detectCores
#' @importFrom xcms CentWaveParam ObiwarpParam PeakDensityParam MatchedFilterParam PeakGroupsParam
#' @importFrom erah setDecPar setAlPar
#' @export

profileParameters <- function(technique) {
  parameters <- new('ProfileParameters',
                    technique = technique,
                    processingParameters = list()
  )
  
  if (technique == 'GCMS-eRah') {
    parameters@processingParameters <- list(
      info = list(cls = 'class'),
      deconvolution = setDecPar(min.peak.width = 1, avoid.processing.mz = c(54:69,73:75,147:149)),
      alignment = setAlPar(min.spectra.cor = 0.90, max.time.dist = 1, mz.range = 70:600),
      identification = list(DB = 'golm',
                            path = 'GMD_20111121_VAR5_ALK_MSP.txt',
                            DBname = 'GMD', 
                            DBversion = 'GMD_20111121',
                            DBinfo = "GOLM Metabolome Database
                                      ------------------------
                                      Kopka, J., Schauer, N., Krueger, S., Birkemeyer, C., Usadel, B., Bergmuller, E., Dor-
                                      mann, P., Weckwerth, W., Gibon, Y., Stitt, M., Willmitzer, L., Fernie, A.R. and Stein-
                                      hauser, D. (2005) GMD.CSB.DB: the Golm Metabolome Database, Bioinformatics, 21, 1635-
                                      1638.",
                            type = 'VAR5.ALK'
                            )
    )
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
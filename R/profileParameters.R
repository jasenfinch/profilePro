#' Available techniques
#' @description List techniques available for processing.
#' @return A character vector of techniques.
#' @export

availableTechinques <- function(){
  c('GCMS-eRah','GCMS-XCMS','LCMS-NP','LCMS-RP')
}

#' profileParameters
#' @description Initiate default processing parameters for a given profiling technique.
#' @param technique the profiling technique for which to initiate parameters
#' @importFrom parallel detectCores
#' @importFrom xcms CentWaveParam ObiwarpParam PeakDensityParam MatchedFilterParam PeakGroupsParam FillChromPeaksParam
#' @importFrom erah setDecPar setAlPar
#' @export

profileParameters <- function(technique) {
  
  parameters <- new('ProfileParameters')
  technique(parameters) <- technique
  
  if (technique == 'GCMS-eRah') {
    
    cr <- formals(recMissComp)[-1]
    cr$min.samples <- 1
    
    processingParameters(parameters) <- list(
      cls = 'class',
      parallel = list(nCores = detectCores() * 0.75,clusterType = cltype()),
      info = list(cls = 'class'),
      deconvolution = setDecPar(min.peak.width = 1.2, avoid.processing.mz = c(73:75,147:149)),
      alignment = setAlPar(min.spectra.cor = 0.90, max.time.dist = 3, mz.range = 70:600),
      compoundRecovery = cr,
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
    processingParameters(parameters) <- list(
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
      infilling = FillChromPeaksParam(),
      nCores = {detectCores() * 0.75} %>% round()
    )
  }
  
  if (technique == 'LCMS-RP') {
    processingParameters(parameters) <- list(
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
      infilling = FillChromPeaksParam(),
      nCores = {detectCores() * 0.75} %>% round()
    )
  } 
  
  if (technique == 'LCMS-NP') {
    processingParameters(parameters) <- list(
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
      infilling = FillChromPeaksParam(),
      nCores = {detectCores() * 0.75} %>% round()
    )
  }
  
  return(parameters)
}

cltype <- function(){
  if (.Platform$OS.type == 'windows') {
    type <- 'PSOCK'
  } else {
    type <- 'FORK'
  }
  return(type)
}
#' profileParameters
#' @description Initiate default processing parameters for a given profiling technique.
#' @param technique the profiling technique for which to initiate parameters
#' @importFrom parallel detectCores
#' @importFrom xcms CentWaveParam ObiwarpParam PeakDensityParam MatchedFilterParam PeakGroupsParam FillChromPeaksParam
#' @importFrom erah setDecPar setAlPar
#' @export

profileParameters <- function(technique = NULL) {
  
  availTechniques <- c('GCMS-eRah','GCMS-XCMS','LCMS-NP','LCMS-RP')
  if (is.null(technique)) {
    availTechniques <- paste(availTechniques,collapse = '\n\t\t\t')
    availTechniques <- paste('\n\t\t\t',availTechniques,sep = '')
    cat('\nAvailable Techniques:',availTechniques,sep = '')
  } else {
    parameters <- new('ProfileParameters',
                      technique = technique,
                      processingParameters = list()
    )
    
    if (technique == 'GCMS-eRah') {
      
      cr <- formals(recMissComp)[-1]
      cr$min.samples <- 1
        
      parameters@processingParameters <- list(
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
      parameters@processingParameters <- list(
        info = list(names = 'name', cls = 'class'),
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
        infilling = FillChromPeaksParam(),
        nCores = {detectCores() * 0.75} %>% round()
      )
    }
    
    if (technique == 'LCMS-RP' | technique == 'LCMS-NP') {
      parameters@processingParameters <- list(
        info = list(names = 'name', cls = 'class'),
        peakDetection = CentWaveParam(
          ppm = 1.5,
          peakwidth = c(2,40),
          snthresh = 3.0,
          mzCenterFun = "apex", 
          mzdiff = 0.001, 
          fitgauss = FALSE,
          integrate = 2
        ), 
        retentionTimeCorrection = ObiwarpParam(),
        grouping = PeakDensityParam(
          sampleGroups = 'class',
          bw = 5,
          binSize = 0.015, 
          minFraction = 2/3
        ),
        infilling = FillChromPeaksParam(),
        nCores = {detectCores() * 0.75} %>% round()
      )
    }
    
    return(parameters)
  }
}

cltype <- function(){
  if (.Platform$OS.type == 'windows') {
    type <- 'PSOCK'
  } else {
    type <- 'FORK'
  }
  return(type)
}
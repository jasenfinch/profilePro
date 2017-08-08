#' profileParameters
#' @importFrom parallel detectCores
#' @export

profileParameters <- function(technique) {
  parameters <- new('ProfileParameters',
                    technique = technique,
                    infoName = 'runinfo.csv',
                    processingParameters = list()
                    )
  
  if (technique == 'GCMS-eRah') {
    
  }
  
  if (technique == 'GCMS-XCMS') {
    parameters@processingParameters <- list(
     peakDetection = list(method = 'matchedFilter',
                          fwhm = 3, 
                          snthresh = 1,
                          snames = 'names', 
                          sclass = 'class'
                          ),
     grouping = list(bw = 5,
                     minfrac = 0
                     ),
     retentionTimeCorrection = list(method = "loess"),
     nCores = detectCores()
    )
  }
  
  if (technique == 'LCMS-RP' | technique == 'LCMS-NP') {
    parameters@processingParameters <- list(
      modes = c('neg','pos'),
      peakDetection = list(method = "centWave",
                           ppm = 1.5,
                           peakwidth = c(2,40),
                    snthresh = 3.0,
                    snames = 'names', 
                    sclass = 'class',
                    mzCenterFun = "apex", 
                    mzdiff = 0.05, 
                    fitgauss = FALSE,
                    integrate = 2
                    ), 
      retentionTimeCorrection = list(method = "obiwarp", 
                                     profStep = 1.0
                                     ),
      grouping = list(bw = 5 ,
                      mzwid = 0.015, 
                      minfrac = 0
                      ),
      nCores = detectCores()
      )
  }
  
  return(parameters)
}
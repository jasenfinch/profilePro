
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = erahProcessing,
    
    `GCMS-XCMS` = GCMSprocessing,
    
    `LCMS-RP` = XCMSprocessing,
    
    `LCMS-NP` = XCMSprocessing
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
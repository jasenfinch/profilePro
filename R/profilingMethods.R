
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = XCMSprocessing,
    
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
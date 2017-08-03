
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = function(x){
      
    },
    
    `LCMS-RP` = XCMSlcProcessing,
    
    `LCMS-NP` = XCMSlcProcessing
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
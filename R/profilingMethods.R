
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = function(x){
      
    },
    
    `LCMS-RP` = function(x){
      XCMSlcProcessing(x)
    },
    
    `LCMS-NP` = function(x){
      XCMSlcProcessing(x)
    }
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
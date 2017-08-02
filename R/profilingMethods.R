
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = function(x){
      
    },
    
    `LCMS-RP` = function(x){
      parameters <- x@processingParameters
      # peak_det <- 
    },
    
    `LCMS-NP` = function(x){
      
    }
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
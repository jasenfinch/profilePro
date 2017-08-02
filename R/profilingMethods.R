
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = function(x){
      
    },
    
    `LCMS-C18` = function(x){
      
    },
    
    `LCMS-HILIC` = function(x){
      
    }
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
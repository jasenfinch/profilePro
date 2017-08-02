#' profileParameters
#' @export

profileParameters <- function(technique) {
  parameters <- new('ProfileParameters',
                    technique = technique,
                    processingParameters = list()
                    )
  
  if (technique == 'GCMS-eRah') {
    
  }
  
  if (technique == 'GCMS-XCMS') {
    
  }
  
  if (technique == 'LCMS-C18') {
    
  }
  
  if (technique == 'LCMS-HILIC') {
    
  }
  
  return(parameters)
}
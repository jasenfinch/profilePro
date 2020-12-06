
checkParameters <- function(object){
  errors <- character()
  
  if (!(object@technique %in% availableTechinques())) {
    msg <- str_c('Technique should be one of ',
                 str_c(availableTechinques(),collapse = ', '),
                 '.')
    
    errors <- c(errors,msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}

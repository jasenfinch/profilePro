#' @importFrom xcms featureValues featureDefinitions
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%

createXCMSpeakTable <- function(Data,mode = NULL){
  m <- mode
  if (grepl('n',m)) {
    m <- 'n'
  }
  if (grepl('p',m)) {
    m <- 'p'
  }
  values <- featureValues(Data[[mode]]) %>% t() %>% as_tibble
  definitions <- featureDefinitions(Data[[mode]]) %>% 
    as_tibble() %>% 
    mutate(ID = colnames(values),rtmed = rtmed/60, rtmin = rtmin/60, rtmax = rtmax/60)
  
  ID <- str_c(m, round(definitions$mzmed,5), '@', round(definitions$rtmed,3))
  
  colnames(values) <- ID
  values[is.na(values)] <- 0
  return(list(values = values, definitions = definitions))
}

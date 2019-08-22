#' @importFrom xcms featureValues featureDefinitions
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%

createXCMSpeakTable <- function(Data,mode = NA){
  if (grepl('n',mode)) {
    m <- 'n'
  }
  if (grepl('p',mode)) {
    m <- 'p'
  }
  if (is.na(mode)) {
    m <- ''
  }
  
  if (!is.na(mode)) {
    values <- featureValues(Data[[mode]],value = 'into') %>% t() %>% as_tibble   
    definitions <- featureDefinitions(Data[[mode]]) %>% 
      as_tibble() %>% 
      mutate(ID = colnames(values),rtmed = rtmed/60, rtmin = rtmin/60, rtmax = rtmax/60)
  } else {
    values <- featureValues(Data,value = 'into') %>% t() %>% as_tibble
    definitions <- featureDefinitions(Data) %>% 
      as_tibble() %>% 
      mutate(ID = colnames(values),rtmed = rtmed/60, rtmin = rtmin/60, rtmax = rtmax/60)
  }
 
  
  
  ID <- str_c(m, round(definitions$mzmed,5), '@', round(definitions$rtmed,3))
  
  colnames(values) <- ID
  values[is.na(values)] <- 0
  
  definitions <- definitions %>%
    mutate(Feature = !!ID) %>%
    select(Feature,mzmin:ID)
  
  return(list(values = values, definitions = definitions))
}

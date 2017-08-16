#' @importFrom xcms peakTable
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
  p <- featureDefinitions(Data[[mode]]) %>%
    as_tibble() %>%
    mutate(rt = rtmed/60, rtmin = rtmin/60, rtmax = rtmax/60)
  
  ID <- p %>%
    mutate(ID = str_c(m,round(mzmed,5),'@',round(rtmed,3))) %>%
    select(ID)
  
  p <- bind_cols(ID,p)
  p <- p[!duplicated(select(p,ID,sample)),] 
  return(p)
}
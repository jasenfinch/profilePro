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
  p <- peakTable(Data[[mode]]) %>% 
    as_tibble() %>%
    mutate(rt = rt/60,rtmin = rtmin/60,rtmax = rtmax/60)
  ID <- p %>% 
    mutate(ID = str_c(m,round(mz,5),'@',round(rt,3))) %>%
    select(ID)
  p <- bind_cols(ID,p)
  return(p)
}
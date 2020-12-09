#' @importFrom purrr map
#' @importFrom MSnbase readMSData
#' @importMethodsFrom MSnbase polarity filterPolarity
#' @importFrom xcms findChromPeaks adjustRtime groupChromPeaks fillChromPeaks
#' @importFrom BiocParallel bpparam register bpworkers<-
#' @importFrom utils capture.output
#' @importFrom crayon green
#' @importFrom tibble deframe

setMethod('XCMSprocessing',signature = 'MetaboProfile',
          function(x){

            info <- new('NAnnotatedDataFrame',
                        data.frame(
                          sample_name = sampleInfo(x)[,processingParameters(x)$info$name] %>%
                            deframe(),
                          sample_groups = sampleInfo(x)[,processingParameters(x)$info$cls] %>%
                            deframe(),
                          stringsAsFactors = FALSE))
            
            processingParameters(x)$grouping@sampleGroups <- info$sample_groups
            
            if (length(filePaths(x)) < processingParameters(x)$nCores) {
              nCores <- x %>%
                filePaths() %>%
                length()
            } else {
              nCores <- x %>%
                processingParameters() %>%
                .$nCores
            }
            
            para <- bpparam()
            bpworkers(para) <- nCores
            register(para)
            
            message('Reading data')
            
            rawData <- readMSData(filePaths(x),pdata = info, mode = 'onDisk')
            
            modes <- rawData %>%
              polarity() %>%
              unique() %>%
              {.[. != -1]}
            
            processed <- map(modes, ~{
              
              if (!is.na(.x)){
                message()
                
                if (.x == 0) {
                  message(blue('Negative mode'))
                } 
                
                if (.x == 1) {
                  message(red('Positive mode'))
                }
                
                d <- rawData %>%
                  filterPolarity(polarity = .x)
              } else {
                d <- rawData
              }
              
              message(green('Peak picking'))
              d <- d %>%
                findChromPeaks(processingParameters(x)$peakDetection)
              
              message(green('Retention time correction'))
              d <- d %>%
                adjustRtime(processingParameters(x)$retentionTimeCorrection)
              
              message(green('Grouping'))
              d <- d %>%
                groupChromPeaks(processingParameters(x)$grouping)
              
              message(green('Infilling'))
              d <- d %>%
                fillChromPeaks(processingParameters(x)$infilling)
              
              return(d)
            }) 
            
            ms <- modes
            ms[ms == 0] <- 'n'
            ms[ms == 1] <- 'p'
            
            names(processed) <- ms
            
            pt <- map(names(processed),
                      createXCMSpeakTable,
                      processed = processed) %>%
              set_names(ms) 
            
            
            
            processedData(x) <- map(pt,~{
              .$values
            })
            
            peak_info <- map(pt,~{
              .$definitions
            }) %>%
              bind_rows(.id = 'polarity')
            
            processingResults(x) <- list(processed = processed,
                                        peak_info = peak_info
            )
            return(x)
          }
)

#' @importFrom xcms featureValues featureDefinitions
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%

createXCMSpeakTable <- function(processed,mode = NA){
  
  if (is.na(mode)) {
    d <- processed[[1]]
    m <- ''
  } else {
    d <- processed[[mode]]
    m <- mode
  }
  
    values <- featureValues(d,value = 'into') %>% 
      t() %>% 
      as_tibble()   
    definitions <- featureDefinitions(d) %>% 
      as_tibble() %>% 
      mutate(ID = colnames(values),
             rtmed = rtmed/60, 
             rtmin = rtmin/60, 
             rtmax = rtmax/60)
  
  ID <- str_c(m, round(definitions$mzmed,5), '@', round(definitions$rtmed,3))
  
  colnames(values) <- ID
  values[is.na(values)] <- 0
  
  definitions <- definitions %>%
    mutate(feature = !!ID) %>%
    select(feature,mzmin:ID)
  
  return(list(values = values, definitions = definitions))
}

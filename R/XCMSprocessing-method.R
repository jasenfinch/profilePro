#' @importFrom purrr map
#' @importFrom MSnbase readMSData
#' @importMethodsFrom MSnbase polarity filterPolarity
#' @importFrom xcms findChromPeaks adjustRtime groupChromPeaks fillChromPeaks
#' @importFrom BiocParallel bpparam register
#' @importFrom utils capture.output
#' @importFrom crayon green

setMethod('XCMSprocessing',signature = 'MetaboProfile',
          function(x){
            parameters <- processingParameters(x)
            
            info <- new('NAnnotatedDataFrame',
                        data.frame(sample_name = x@Info[,parameters@processingParameters$info$names] %>% unlist(),
                                   sample_groups = x@Info[,parameters@processingParameters$info$cls] %>% unlist(),
                                   stringsAsFactors = F))
            
            parameters@processingParameters$grouping@sampleGroups <- info$sample_groups
            
            if (length(x@files) < parameters@processingParameters$nCores) {
              nCores <- length(x@files)
            } else {
              nCores <- parameters@processingParameters$nCores
            }
            para <- bpparam()
            para@.xData$workers <- nCores
            register(para)
            
            message('Reading data')
            
            rawData <- readMSData(x@files,pdata = info, mode = 'onDisk')
            
            modes <- rawData %>%
              polarity() %>%
              unique() %>%
              {.[. != -1]}
            
            processed <- map(modes, ~{
              message()
              
              if (.x == 0) {
                message(blue('Negative mode'))
              } else {
                message(red('Positive mode'))
              }
              
              d <- rawData %>%
                filterPolarity(polarity = .x)
              
              message(green('Peak picking'))
              d <- d %>%
                findChromPeaks(parameters@processingParameters$peakDetection)
              
              message(green('Retention time correction'))
              d <- d %>%
                adjustRtime(parameters@processingParameters$retentionTimeCorrection)
              
              message(green('Grouping'))
              d <- d %>%
                groupChromPeaks(parameters@processingParameters$grouping)
              
              message(green('Infilling'))
              d <- d %>%
                fillChromPeaks(parameters@processingParameters$infilling)
              
              return(d)
            }) 
            
            ms <- modes
            ms[ms == 0] <- 'n'
            ms[ms == 1] <- 'p'
            
            names(processed) <- ms
            
            pt <- map(ms,createXCMSpeakTable,Data = processed) %>%
              set_names(ms)
            
            Data <- map(pt,~{
              .$values
            })
            
            x@Data <- Data
            x@processingResults <- list(processed = processed,
                                        peakInfo = pt
            )
            return(x)
          }
)
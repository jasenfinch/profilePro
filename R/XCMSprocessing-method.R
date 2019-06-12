#' @importFrom purrr map
#' @importMethodsFrom MSnbase filterPolarity polarity
#' @importFrom xcms findChromPeaks adjustRtime groupChromPeaks fillChromPeaks
#' @importFrom BiocParallel bpparam register
#' @importFrom utils capture.output

setMethod('XCMSprocessing',signature = 'MetaboProfile',
          function(x){
            parameters <- x@processingParameters
            
            info <- new('NAnnotatedDataFrame',data.frame(sample_name = x@Info[,parameters@processingParameters$info$names] %>% unlist(),sample_groups = x@Info[,parameters@processingParameters$info$cls] %>% unlist(),stringsAsFactors = F))
            
            parameters@processingParameters$grouping@sampleGroups <- info$sample_groups
            
            if (length(x@files) < parameters@processingParameters$nCores) {
              nCores <- length(x@files)
            } else {
              nCores <- parameters@processingParameters$nCores
            }
            para <- bpparam()
            para@.xData$workers <- nCores
            register(para)
            
            rawData <- readMSData(x@files,pdata = info, mode = 'onDisk')
            
            modes <- rawData %>%
              polarity() %>%
              unique()
            
            processed <- map(modes, ~{
              m <- .
              rawData %>%
                filterPolarity(polarity = m) %>%
                findChromPeaks(parameters@processingParameters$peakDetection) %>%
                adjustRtime(parameters@processingParameters$retentionTimeCorrection) %>%
                groupChromPeaks(parameters@processingParameters$grouping) %>%
                fillChromPeaks(parameters@processingParameters$infilling)
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
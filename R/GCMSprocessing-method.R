
setMethod('GCMSprocessing',signature = 'MetaboProfile',
          function(x){
            parameters <- x@processingParameters
            
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
            
            rawData <- readMSData(x@files,pdata = info, mode = 'onDisk')
            
            processed <- rawData %>%
              findChromPeaks(parameters@processingParameters$peakDetection) %>%
              groupChromPeaks(parameters@processingParameters$grouping) %>%
              adjustRtime(parameters@processingParameters$retentionTimeCorrection) %>%
              groupChromPeaks(parameters@processingParameters$grouping) %>%
              fillChromPeaks(parameters@processingParameters$infilling)
            
            pt <- createXCMSpeakTable(processed)
            
            Data <- pt$values
            
            x@Data <- Data
            x@processingResults <- list(processed = processed,
                                        peakInfo = pt
            )
            return(x)
          }
)

setMethod('GCMSprocessing',signature = 'MetaboProfile',
          function(x){
            
            info <- new('NAnnotatedDataFrame',
                        data.frame(
                          sample_name = sampleInfo(x)[,processingParameters(x)$info$name] %>%
                            deframe(),
                          sample_groups = sampleInfo(x)[,processingParameters(x)$info$cls] %>%
                            deframe(),
                          stringsAsFactors = FALSE))
            
            processingParameters(x)$grouping@sampleGroups <- info$sample_groups
            
            register(FutureParam()) 
            
            message('Reading data')
            d <- readMSData(filePaths(x),pdata = info, mode = 'onDisk')
            
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
            
            peak_info <- d %>%
              list() %>%
              createXCMSpeakTable()
            
            processedData(x) <- peak_info$values
            
            processingResults(x) <- list(processed = d,
                                        peak_info = peak_info$definitions
            )
            return(x)
          }
)
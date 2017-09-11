#' @importFrom purrr map
#' @importFrom MSnbase readMSData
#' @importFrom xcms findChromPeaks adjustRtime groupChromPeaks
#' @importFrom BiocParallel bpparam register
#' @importFrom utils capture.output

setMethod('XCMSprocessing',signature = 'MetaboProfile',
          function(x){
            parameters <- x@processingParameters
            if (is.null(names(x@files))) {
              modes <- NA
            } else {
              modes <- names(x@files)
            }
            
            info <- new('NAnnotatedDataFrame',data.frame(sample_name = x@Info[,parameters@processingParameters$info$names],sample_groups = x@Info[,parameters@processingParameters$info$cls],stringsAsFactors = F))
            
            parameters@processingParameters$grouping@sampleGroups <- unlist(x@Info[,parameters@processingParameters$info$cls])
            
            para <- bpparam()
            para@.xData$workers <- parameters@processingParameters$nCores
            register(para)
            
            processed <- map(modes, ~{
              files <- x@files[[grep(.,names(x@files))]]
              rawData <- readMSData(files,pdata = info, mode = 'onDisk')
              x <- findChromPeaks(rawData,parameters@processingParameters$peakDetection)
              return(x)
            }) %>%
              map(adjustRtime,
                  param = parameters@processingParameters$retentionTimeCorrection
              ) %>%
              map(groupChromPeaks,
                  param = parameters@processingParameters$grouping
              )
            
            names(processed) <- modes
            
            pt <- map(modes,createXCMSpeakTable,Data = processed)
            names(pt) <- modes
            
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
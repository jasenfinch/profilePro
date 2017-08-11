#' @importFrom purrr map
#' @importFrom MSnbase readMSData2
#' @importFrom xcms findChromPeaks adjustRtime groupChromPeaks
#' @importFrom BiocParallel bpparam register
#' @importFrom utils capture.output

setMethod('XCMSlcProcessing',signature = 'MetaboProfile',
          function(x){
            parameters <- x@processingParameters
            modes <- parameters@processingParameters$modes
            
            info <- new('NAnnotatedDataFrame',data.frame(sample_name = x@Info[,parameters@processingParameters$info$names],sample_groups = x@Info[,parameters@processingParameters$info$cls],stringsAsFactors = F))
            
            parameters@processingParameters$grouping@sampleGroups <- info@data$Status
            
            para <- bpparam()
            para@.xData$workers <- parameters@processingParameters$nCores
            register(para)
            
            processed <- map(modes, ~{
              files <- x@files[grepl(.,x@files)]
              rawData <- readMSData2(files,pdata = info)
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
            
            #suppressMessages(filled <- map(groups,fillPeaks))
            filled <- groups
            
            pt <- map(modes,createXCMSpeakTable,Data = filled)
            names(pt) <- modes
            
            Data <- map(pt,~{
              ncls <- length(unique(unlist(x@Info[,parameters@processingParameters$peakDetection$sclass])))
              dat <- .[,(9 + ncls):ncol(.)] %>%
                t()
              colnames(dat) <- .$ID
              dat[is.na(dat)] <- 0
              dat <- as_tibble(dat)
              return(dat)
            })
            
            x@Data <- Data
            x@processingResults <- list(peakDetection = peakDetection, 
                                        retentionTimeCorrection = retentionTimeCorrection, 
                                        groups = groups,
                                        # filled = filled,
                                        peakTable = pt
            )
            return(x)
          }
)
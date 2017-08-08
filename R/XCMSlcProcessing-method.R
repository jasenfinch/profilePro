#' @importFrom purrr map
#' @importFrom xcms xcmsSet retcor group fillPeaks
#' @importFrom BiocParallel bpparam
#' @importFrom utils capture.output

setMethod('XCMSlcProcessing',signature = 'MetaboProfile',
          function(x){
            parameters <- x@processingParameters
            modes <- parameters@processingParameters$modes
            
            suppressPackageStartupMessages(peakDetection <- map(modes, ~{
              files <- x@files[grepl(.,x@files)]
              peakDet <- xcmsSet
              parameters@processingParameters$peakDetection$snames <- unlist(x@Info[,parameters@processingParameters$peakDetection$snames])
              parameters@processingParameters$peakDetection$sclass <- unlist(x@Info[,parameters@processingParameters$peakDetection$sclass])
              f <- formals(peakDet)
              f[names(parameters@processingParameters$peakDetection)] <- parameters@processingParameters$peakDetection
              formals(peakDet) <- f
              para <- bpparam()
              para@.xData$workers <- parameters@processingParameters$nCores
              formals(peakDet)$BPPARAM <- para
              x <- peakDet(files = files)
              return(x)
            }))
            names(peakDetection) <- modes
            
            suppressMessages(capture.output(retentionTimeCorrection <- map(peakDetection,retcor, 
                                                                           method = parameters@processingParameters$retentionTimeCorrection$method, 
                                                                           profStep = parameters@processingParameters$retentionTimeCorrection$profStep
            )))
            
            suppressMessages(groups <- map(retentionTimeCorrection,group,
                                           bw = parameters@processingParameters$grouping$bw,
                                           mzwid = parameters@processingParameters$grouping$mzwid,
                                           minfrac = parameters@processingParameters$grouping$minfrac
            ))
            
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
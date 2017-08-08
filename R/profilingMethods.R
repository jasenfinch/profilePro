
profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      
    },
    
    `GCMS-XCMS` = function(x){
      parameters <- x@processingParameters
      info <- x@Info
      peakDet <- xcmsSet
      parameters@processingParameters$peakDetection$snames <- unlist(info[,parameters@processingParameters$peakDetection$snames])
      parameters@processingParameters$peakDetection$sclass <- unlist(info[,parameters@processingParameters$peakDetection$sclass])
      f <- formals(peakDet)
      f[names(parameters@processingParameters$peakDetection)] <- parameters@processingParameters$peakDetection
      formals(peakDet) <- f
      para <- bpparam()
      para@.xData$workers <- parameters@processingParameters$nCores
      formals(peakDet)$BPPARAM <- para
      
      peakDetection <- peakDet(files = x@files)
      
      suppressMessages(groups <- group(peakDetection,
                                       bw = parameters@processingParameters$grouping$bw,
                                       minfrac = parameters@processingParameters$grouping$minfrac
      ))
      
      suppressMessages(retentionTimeCorrection <- retcor(groups, 
                                                         method = parameters@processingParameters$retentionTimeCorrection$method))
      
      suppressMessages(groups <- group(retentionTimeCorrection,
                                       bw = parameters@processingParameters$grouping$bw,
                                       minfrac = parameters@processingParameters$grouping$minfrac
      ))
      
      # suppressMessages(filled <- fillPeaks(groups))
      
      pt <- peakTable(groups) %>% 
        as_tibble() %>%
        mutate(rt = rt/60,rtmin = rtmin/60,rtmax = rtmax/60)
      ID <- pt %>% 
        mutate(ID = str_c(mz,'@',round(rt,3))) %>%
        select(ID)
      pt <- bind_cols(ID,pt)
      
      
      ncls <- length(unique(unlist(parameters@processingParameters$peakDetection$sclass)))
      Data <- pt[,(9 + ncls):ncol(pt)] %>%
        t()
      colnames(Data) <- pt$ID
      Data[is.na(Data)] <- 0
      Data <- as_tibble(Data)
      
      x@Data <- list(Data = Data)
      x@processingResults <- list(peakDetection = peakDetection, 
                                  retentionTimeCorrection = retentionTimeCorrection,
                                  groups = groups,
                                  # filled = filled,
                                  peakTable = pt
      )
      return(x)
    },
    
    `LCMS-RP` = XCMSlcProcessing,
    
    `LCMS-NP` = XCMSlcProcessing
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
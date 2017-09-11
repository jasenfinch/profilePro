#' @importFrom stringr str_split
#' @importFrom erah importGMD identifyComp idList createdt newExp deconvolveComp alignComp
#' @importFrom dplyr left_join
#' @importFrom utils capture.output


profilingMethods <- function(method = NULL){
  
  methods <- list(
    
    `GCMS-eRah` = function(x){
      folder <- unlist(x@files)[1] %>%
        str_split('/')
      folder <- folder[[1]][-length(folder[[1]])] 
      folderName <- folder[length(folder) - 1]
      folder <- str_c(folder,collapse = '/')
      
      createdt(folder)
      
      ex <- newExp(instrumental = str_c(folder,str_c(folderName,'_inst.csv')), 
                   phenotype = str_c(folder,str_c(folderName,'_pheno.csv')))
      
      capture.output(ex <- deconvolveComp(ex, x@processingParameters@processingParameters$deconvolution))
      
      capture.output(ex <- alignComp(ex, x@processingParameters@processingParameters$alignment))
      
      if (x@processingParameters@processingParameters$identification$DB == 'golm') {
        golm.database <- importGMD(filename = x@processingParameters@processingParameters$identification$path, 
                                   DB.name = x@processingParameters@processingParameters$identification$DBname, 
                                   DB.version = x@processingParameters@processingParameters$identification$DBversion, 
                                   DB.info = x@processingParameters@processingParameters$identification$DBinfo, 
                                   type = x@processingParameters@processingParameters$identification$type)
        mslib <- golm.database
      }
      
      capture.output(ex <- identifyComp(ex,id.database = mslib))
      
      id.list <- idList(ex,id.database = mslib)
      
      feat <- suppressMessages(left_join(ex@Results@Alignment,id.list))
      feat <- str_c(feat$Factor,'|',feat$Name.1,sep = ' ')
      
      Data <- ex@Results@Alignment[,6:ncol(ex@Results@Alignment)] %>%
        t() %>%
        as_tibble()
      colnames(Data) <- feat
      
      system(str_c('rm ',folder,str_c(folderName,'_inst.csv')))
      system(str_c('rm ',folder,str_c(folderName,'_pheno.csv')))
      
      x@Data <- Data
      x@processingResults <- list(processed = ex)
      return(x)
    },
    
    `GCMS-XCMS` = XCMSprocessing,
    
    `LCMS-RP` = XCMSprocessing,
    
    `LCMS-NP` = XCMSprocessing
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  return(method)
}
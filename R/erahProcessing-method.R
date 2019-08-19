#' @importFrom erah importGMD identifyComp idList newExp deconvolveComp alignComp createInstrumentalTable dataList recMissComp createPhenoTable
#' @importFrom dplyr left_join
#' @importFrom tidyr spread gather
#' @importFrom tibble deframe

setMethod('erahProcessing',signature = 'MetaboProfile',
         function(x){
           
           if (x@processingParameters@processingParameters$identification$DB == 'golm') {
             suppressWarnings(golm.database <- importGMD(filename = x@processingParameters@processingParameters$identification$path, 
                                                         DB.name = x@processingParameters@processingParameters$identification$DBname, 
                                                         DB.version = x@processingParameters@processingParameters$identification$DBversion, 
                                                         DB.info = x@processingParameters@processingParameters$identification$DBinfo, 
                                                         type = x@processingParameters@processingParameters$identification$type))
             mslib <- golm.database
           }
           
           instrumental <- createInstrumentalTable(x@files)
           
           phenotype <- createPhenoTable(x@files,x@Info[,x@processingParameters@processingParameters$cls] %>% deframe())
           
           ex <- newExp(instrumental = instrumental, 
                        phenotype = phenotype) %>%
             deconvolveComp(x@processingParameters@processingParameters$deconvolution,
                            parallel = x@processingParameters@processingParameters$parallel) %>%
             alignComp(x@processingParameters@processingParameters$alignment) %>%
             recMissComp(min.samples = x@processingParameters@processingParameters$compoundRecovery$min.samples,
                         free.model = x@processingParameters@processingParameters$compoundRecovery$free.model) %>%
             identifyComp(id.database = golm.database)
           
           Data <- dataList(ex,, id.database = mslib) %>%
             mutate(Name = str_c('Feature ',AlignID,' ',Name.1)) %>%
             select(Name,phenotype$sampleID[1]:phenotype$sampleID[nrow(phenotype)]) %>%
             gather(Sample,Intensity,-Name) %>%
             spread(Name,Intensity) %>%
             select(-Sample)
           
           x@Data <- Data
           x@processingResults <- list(processed = ex)
           return(x)
         }
)
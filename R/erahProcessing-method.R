#' @importFrom erah importGMD identifyComp idList newExp deconvolveComp alignComp createInstrumentalTable dataList recMissComp createPhenoTable
#' @importFrom dplyr left_join
#' @importFrom tidyr spread gather
#' @importFrom tibble deframe

setMethod('erahProcessing',signature = 'MetaboProfile',
          function(x){
            
            compound_database <- processingParameters(x)$identification$compound_database
            
            instrumental <- x %>%
              filePaths() %>%
              createInstrumentalTable()
            
            phenotype <- createPhenoTable(filePaths(x),
                                          sampleInfo(x)[,processingParameters(x)$cls] %>% 
                                            deframe())
            
            ex <- newExp(instrumental = instrumental, 
                         phenotype = phenotype)
            
            message(green('Deconvolution'))
            ex <- ex %>%
              deconvolveComp(processingParameters(x)$deconvolution,
                             parallel = processingParameters(x)$parallel)
            
            message(green('Alignment'))
            ex <- ex %>%
              alignComp(processingParameters(x)$alignment)
            
            message(green('Missing compound recovery'))
            ex <-  ex %>%
              recMissComp(min.samples = processingParameters(x)$compoundRecovery$min.samples,
                          free.model = processingParameters(x)$compoundRecovery$free.model)
            
            message(green('Compound identification'))
            ex <- ex %>%
              identifyComp(id.database = compound_database)
            
            processed_data <- dataList(ex,, id.database = compound_database) %>%
              mutate(Name = str_c('Feature ',AlignID,' ',Name.1)) %>%
              select(Name,phenotype$sampleID[1]:phenotype$sampleID[nrow(phenotype)]) %>%
              gather(Sample,Intensity,-Name) %>%
              spread(Name,Intensity) %>%
              select(-Sample)
            
            processedData(x) <- processed_data
            processingResults(x) <- list(processed = ex)
            return(x)
          }
)
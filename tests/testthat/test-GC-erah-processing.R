## Process example GC data
library(erah)
library(gcspikelite)
data('targets')

GC_file_paths <- list.files(
  system.file('data',
              package = 'gcspikelite'),
  pattern = '.CDF',
  full.names = TRUE)[1:2]

GC_sample_info <- tibble(
  fileOrder = seq_along(GC_file_paths),
  injOrder = seq_along(GC_file_paths),
  fileName = basename(GC_file_paths),
  batch = 1,
  block = 1) %>% 
  mutate(name = tools::file_path_sans_ext(fileName),
         class = targets$Group[1:2])

GC_parameters <- profileParameters('GCMS-eRah',nCores = 2)
processingParameters(GC_parameters)$identification <- list(
  DB = 'MassBank',
  compound_database = mslib)

GC_processed_data <- profileProcess(GC_file_paths,
                                    GC_sample_info,
                                    GC_parameters)

test_that('GC data returned correctly',{
  expect_s4_class(GC_processed_data,'MetaboProfile')
})

test_that('Processed GC data can be extracted',{
  expect_s3_class(processedData(GC_processed_data),'tbl_df')
})

test_that('GC peak info can be extracted',{
  expect_s3_class(peakInfo(GC_processed_data),'tbl_df')
})

test_that('The processing object can be extracted for GC data',{
  expect_s4_class(extractProcObject(GC_processed_data),'MetaboSet')
})

test_that('Chromatogram can be plotted for GC data',{
  expect_null(plotChromatogram(GC_processed_data))
})

test_that('TIC can be plotted for GC data',{
  expect_s3_class(plotTIC(GC_processed_data),'ggplot')
})


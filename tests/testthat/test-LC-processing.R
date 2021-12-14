## Process example LC data
LC_file_paths <- list.files(
  system.file("cdf", 
              package = "faahKO"),
  full.names = TRUE,
  recursive = TRUE)[1:2]

LC_sample_info <- tibble(
  fileOrder = seq_along(LC_file_paths),
  injOrder = seq_along(LC_file_paths),
  fileName = basename(LC_file_paths),
  batch = 1,
  block = 1) %>% 
  mutate(name = tools::file_path_sans_ext(fileName),
         class = substr(name,1,2))

LC_parameters <- profileParameters('LCMS-RP',nCores = 2)
processingParameters(LC_parameters)$peakDetection <- CentWaveParam(
  snthresh = 20, 
  noise = 1000)
processingParameters(LC_parameters)$retentionTimeCorrection <- ObiwarpParam()
processingParameters(LC_parameters)$grouping <- PeakDensityParam(
  sampleGroups = LC_sample_info$class,
  maxFeatures = 300,
  minFraction = 2/3)

LC_processed_data <- profileProcess(
  LC_file_paths,
  LC_sample_info,
  LC_parameters)

test_that("LC data returned correctly", {
  expect_s4_class(LC_processed_data,'MetaboProfile')
})

test_that('Processed LC data can be extracted',{
  expect_type(processedData(LC_processed_data),'list') 
})

test_that('LC peak info can be extracted',{
  expect_s3_class(peakInfo(LC_processed_data),'tbl_df')
})

test_that('The processing object can be extracted from the LC data',{
  expect_type(extractProcObject(LC_processed_data),'list')
})
  
test_that('Chromatogram can be plotted for LC data',{
  pl <- plotChromatogram(LC_processed_data,
                         cls = 'class',
                         group = TRUE)
  
  expect_s3_class(pl,'ggplot')
})

test_that('Chromatogram can be plotted when cls = NULL and group = TRUE',{
  pl <- plotChromatogram(LC_processed_data,
                         cls = NULL,
                         group = TRUE)
  
  expect_s3_class(pl,'ggplot')
})
  
test_that('TIC can be plotted for LC data',{
  expect_s3_class(plotTIC(LC_processed_data),'ggplot')
})


test_that("profileProcess works for LC data", {
  file_paths <- list.files(
    system.file("cdf", 
                package = "faahKO"),
    full.names = TRUE,
    recursive = TRUE)[1:2]
  file_names <- basename(file_paths)
  sample_names <- tools::file_path_sans_ext(file_names)
  
  sample_info <- tibble(fileOrder = seq_along(file_paths),
                           injOrder = seq_along(file_paths),
                           fileName = file_names,
                           batch = 1,
                           block = 1,
                           name = sample_names,
                           class = substr(sample_names,1,2))
  
  parameters <- profileParameters('LCMS-RP',nCores = 2)
  processingParameters(parameters)$peakDetection <- CentWaveParam(snthresh = 20, 
                                                                  noise = 1000)
  processingParameters(parameters)$retentionTimeCorrection <- ObiwarpParam()
  processingParameters(parameters)$grouping <- PeakDensityParam(sampleGroups = sample_info$class,
                                                               maxFeatures = 300,
                                                               minFraction = 2/3)
  
  processed_data <- profileProcess(file_paths,sample_info,parameters)

  expect_s4_class(processed_data,'MetaboProfile')
  expect_equal(class(processedData(processed_data)),'list')
  expect_s3_class(peakInfo(processed_data),'tbl_df')
  expect_equal(class(extractProcObject(processed_data)),'list')
  
  # expect_s3_class(plotChromatogram(processed_data),'ggplot')
  expect_s3_class(plotTIC(processed_data),'ggplot')
})

test_that('profileProcess works for GC data using eRah',{
  library(erah)
  library(gcspikelite)
  data('targets')
  
  file_paths <- list.files(
    system.file('data',
                package = 'gcspikelite'),
    pattern = '.CDF',
    full.names = TRUE)[1:2]
  file_names <- basename(file_paths)
  sample_names <- tools::file_path_sans_ext(file_names)
  
  sample_info <- tibble(fileOrder = seq_along(file_paths),
                        injOrder = seq_along(file_paths),
                        fileName = file_names,
                        batch = 1,
                        block = 1,
                        name = sample_names,
                        class = targets$Group[1:2])
  
  parameters <- profileParameters('GCMS-eRah',nCores = 2)
  processingParameters(parameters)$identification <- list(DB = 'MassBank',
                                                          compound_database = mslib)
  
  processed_data <- profileProcess(file_paths,sample_info,parameters)
  
  expect_s4_class(processed_data,'MetaboProfile')
  expect_s3_class(processedData(processed_data),'tbl_df')
  expect_s3_class(peakInfo(processed_data),'tbl_df')
  expect_s4_class(extractProcObject(processed_data),'MetaboSet')
  
  # expect_null(plotChromatogram(processed_data),'ggplot')
  expect_s3_class(plotTIC(processed_data),'ggplot')
  })

test_that('profileProcess works for GC data using eRah',{
  library(erah)
  library(gcspikelite)
  data('targets')
  
  file_paths <- list.files(
    system.file('data',
                package = 'gcspikelite'),
    pattern = '.CDF',
    full.names = TRUE)[1:2]
  file_names <- basename(file_paths)
  sample_names <- tools::file_path_sans_ext(file_names)
  
  sample_info <- tibble(fileOrder = seq_along(file_paths),
                        injOrder = seq_along(file_paths),
                        fileName = file_names,
                        batch = 1,
                        block = 1,
                        name = sample_names,
                        class = targets$Group[1:2])
  
  parameters <- profileParameters('GCMS-XCMS',nCores = 2)
  
  processed_data <- profileProcess(file_paths,sample_info,parameters)
  
  expect_s4_class(processed_data,'MetaboProfile')
  expect_s3_class(processedData(processed_data),'tbl_df')
  expect_s3_class(peakInfo(processed_data),'tbl_df')
  expect_s4_class(extractProcObject(processed_data),'MetaboSet')
  
  # expect_null(plotChromatogram(processed_data),'ggplot')
  expect_s3_class(plotTIC(processed_data),'ggplot')
})

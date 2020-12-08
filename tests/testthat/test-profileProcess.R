
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
})

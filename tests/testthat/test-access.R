p <- new('ProfileParameters')

test_that('technique can be set and returned',{
  technique(p) <- 'LCMS-RP'
  
  expect_equal(technique(p),'LCMS-RP')
})

test_that('processingParameters can be set and returned',{
  processingParameters(p) <- list(test = 1)
  
  expect_equal(processingParameters(p) %>%
                 names(),'test')
})

d <- new('MetaboProfile',
         technique = 'LCMS-RP',
         file_paths = 'test.mzML',
         sample_info = tibble(
           fileOrder = 1,
           injOrder = 1,
           fileName = 'test.mzML',
           batch = 1,
           block = 1,
           name = 'test',
           class = 'test'
         ))

test_that('file paths can be set and returned',{
  expect_equal(filePaths(d),'test.mzML')
})

test_that('error given when incorrect sample information specified',{
  expect_error({sampleInfo(d) <- tibble()})
})

test_that('sample information can be set',{
  expect_s3_class(sampleInfo(d),'tbl_df')
})

test_that('processing results can be set and returned',{
  processingResults(d) <- list()
  
  expect_equal(class(processingResults(d)),'list')
})

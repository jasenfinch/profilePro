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
         technique = 'LCMS-RP')

test_that('file paths can be set and returned',{
  files(d) <- 'test.mzML'
  expect_equal(files(d),'test.mzML')
})

test_that('Error given when incorrect sample information specified',{
  expect_error({sampleInfo(d) <- tibble()})
})

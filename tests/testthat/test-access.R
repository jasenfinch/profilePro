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

test_that("available techniques returned", {
  techniques <- availableTechinques()
  
  expect_length(techniques,4)
})

test_that('available techniques printed by profileParameters',{
  expect_output(profileParameters(),'Available Techniques:')
})

test_that('profileParamters errors if incorrect technique specified',{
  expect_error(new('ProfileParameters',technique = 'incorrect'))
})

test_that('profileParameters works for GCMS-eRah',{
  p <- profileParameters('GCMS-eRah')
  
  expect_s4_class(p,'ProfileParameters')
})

test_that('profileParameters works for GCMS-XCMS',{
  p <- profileParameters('GCMS-XCMS')
  
  expect_s4_class(p,'ProfileParameters')
})

test_that('profileParameters works for LCMS-NP',{
  p <- profileParameters('LCMS-NP')
  
  expect_s4_class(p,'ProfileParameters')
})

test_that('profileParameters works for LCMS-RP',{
  p <- profileParameters('LCMS-RP')
  
  expect_s4_class(p,'ProfileParameters')
})

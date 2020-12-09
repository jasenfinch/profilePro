
test_that("ProfileParameters show works", {
  p <- new('ProfileParameters')
  
  expect_output(print(p),'Processing parameters for technique')
})

test_that('MetaboProfile show works',{
  d <- new('MetaboProfile')
  
  expect_output(print(d),'profilePro MetaboProfile')
})

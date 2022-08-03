test_that("listCartesian works", {
  
  allList <- list(a=list(1,2), b=list(NULL, 'auto'), c=list(-1))
  
  paramLists <- listCartesian(allList)
  
  expect_equal(length(paramLists), 2*2*1)
  expect_equal(names(paramLists[[1]]), c('a', 'b', 'c'))
  expect_equal(length(paramLists[[1]]), 3)
  
})

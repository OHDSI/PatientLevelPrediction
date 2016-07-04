library("testthat")

test_that("personSplitter splits test train correctly", {

  # this code should return an error due to outcome count when test=0.3 and nfold>1
  population1 <- data.frame(rowId=1:20, outcomeCount=c(1,1,1,1,rep(0,16))) 
  
  # now test the actual fold 
  population2 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158))) 
  
  test1 <- expect_error(personSplitter(population1, test=0.3, nfold=3))
  test2 <- personSplitter(population2, test=0.2, nfold=4)
  
  test2 <- merge(population2, test2)
  test2 <- table(test2$outcomeCount, test2$index)
  test2.returned <- paste(test2, collapse='-')
  test2.expected <- paste(matrix(c(32,32,32,31,31,8,9,9,8,8), ncol=5, byrow=T),collapse='-')
  
  expect_identical(test2.returned, test2.expected)
  is_true(200==sum(test2))
  
})

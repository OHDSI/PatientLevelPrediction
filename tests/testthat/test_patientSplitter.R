library("testthat")

test_that("personSplitter splits test train correctly", {

  # error message checks
  population1 <- data.frame(rowId=1:20, outcomeCount=c(1,1,1,1,rep(0,16))) 
  expect_error(personSplitter(population1, test=0.3, nfold=3, silent=T))
  
  population2 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158)))
  expect_error(personSplitter(population2, test=0.3, nfold=-1, silent=T))
  expect_error(personSplitter(population2, test=1.5, nfold=5, silent=T))
  expect_error(personSplitter(population2, test=-1, nfold=5, silent=T))
  
  # fold creation check 1 (fixed)
  test <- personSplitter(population2, test=0.2, nfold=4, silent=T)
  test <- merge(population2, test)
  test <- table(test$outcomeCount, test$index)
  test.returned <- paste(test, collapse='-')
  test.expected <- paste(matrix(c(32,32,32,31,31,8,9,9,8,8), ncol=5, byrow=T),collapse='-')
  expect_identical(test.returned, test.expected)
  
  # fold creation check 2 (random sum)
  i <- sample(100:100000,1)
  population3 <- data.frame(rowId=1:i, outcomeCount=c(rep(1,floor(i/3)),rep(0,i-floor(i/3)))) 
  test <- personSplitter(population3, test=0.2, nfold=4, silent=T) 
  test <- merge(population3, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(i))

})

test_that("time Splitter splits test train correctly", {
  
  # error message checks
  population2 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158)), 
                            cohortStartDate = as.Date("2016-01-01") + c(1:200))
  expect_error(timeSplitter(population2, test=0.3, nfold=-1, silent=T))
  expect_error(timeSplitter(population2, test=1.5, nfold=5, silent=T))
  expect_error(timeSplitter(population2, test=-1, nfold=5, silent=T))
 
  # fold creation check 2 (random sum)
  size <- 500
  cohortStartDates = Sys.Date() + sort(sample(-250:250,size))
  population <- data.frame(rowId=1:size, outcomeCount=c(rep(1,floor(size/3)),rep(0,size-floor(size/3))),cohortStartDate = cohortStartDates)
  test <- timeSplitter(population, test=0.2, nfold=4, silent=T) 
  test <- merge(population, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(size))
})

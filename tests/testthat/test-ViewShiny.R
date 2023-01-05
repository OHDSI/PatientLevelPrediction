context("ViewShiny")

test_that("server function works", {
  
  connectionShiny <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite', 
    server = tempfile()
    )
  connectionShiny <- ResultModelManager::ConnectionHandler$new(connectionShiny)
  shinyServer <- server(connectionShiny)
  
  testthat::expect_true(inherits(shinyServer, 'function'))
  
})
  
test_that("ui function works", {
  
  shinyUI <- ui()
  testthat::expect_true(inherits(shinyUI, 'shiny.tag'))
  
})
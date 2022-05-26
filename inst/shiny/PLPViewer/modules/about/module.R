aboutViewer <- function(id=1) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shiny::includeMarkdown(path = 'modules/about/about.md')
    )
    
  )
}

aboutServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}

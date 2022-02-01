emptyPlot <- function(title = NULL){
  p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::config(
      displayModeBar = FALSE
    ) %>%
    plotly::layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
  return(p)
} 

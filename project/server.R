server <- function(input, output, session) {
  output$p <- renderPlot({
    plot(rnorm(input$n), rnorm(input$n))
  })
}

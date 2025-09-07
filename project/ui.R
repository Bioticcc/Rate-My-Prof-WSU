library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of points:", min = 10, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("p")
    )
  )
)

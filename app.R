library(shiny)
library(shinymanager)

check_creds <- check_credentials(
  db = "credentials.sqlite",
  passphrase = "change-this-passphrase"
)

ui_raw <- navbarPage(
  title = "Rate-My-Prof-WSU",
  id = "main-nav",
  inverse = TRUE,
  header = tags$head(
    # Google Font for a clean, modern look
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    # App stylesheet
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  tabPanel(
    title = "Home",
    value = "home",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(class = "hero",
              div(class = "hero-content",
                  h1("Find and share insights on WSU professors"),
                  p(class = "subtext", "Search, rate, and review to help Cougs pick the right courses."),
                  div(class = "greet-row",
                      textInput("name", NULL, placeholder = "Your name"),
                      actionButton("go", "Greet", class = "btn-primary")
                  ),
                  uiOutput("greet")
              )
          )
        )
      ),
      fluidRow(
        column(6,
               div(class = "card",
                   h3("Top Professors"),
                   p("Browse the highest-rated professors across departments.")
               )
        ),
        column(6,
               div(class = "card",
                   h3("Trending Courses"),
                   p("See courses with the most recent ratings and reviews.")
               )
        )
      )
    )
  ),

  tabPanel(
    title = "Professors",
    value = "professors",
    fluidPage(
      h2("Professors"),
      p("Explore professor ratings, average difficulty, and student feedback. (Coming soon)")
    )
  ),

  tabPanel(
    title = "Courses",
    value = "courses",
    fluidPage(
      h2("Courses"),
      p("Search and filter courses by department and term. (Coming soon)")
    )
  ),

  tabPanel(
    title = "About",
    value = "about",
    fluidPage(
      h2("About Rate-My-Prof-WSU"),
      p("A community-driven tool for WSU students to find the right classes with confidence."),
      p("This project is not affiliated with Washington State University but aims to follow WSU-inspired branding.")
    )
  ),

  tabPanel(
    title = "Feedback",
    value = "feedback",
    fluidPage(
      h2("Feedback"),
      p("Have ideas or found a bug? Reach out!"),
      tags$ul(
        tags$li(HTML("Open an issue on GitHub: <a href='https://github.com/ThaRealJdion/Rate-My-Prof-WSU' target='_blank'>Repo</a>")),
        tags$li(HTML("Email: <a href='mailto:jaydon.devictoria@wsu.edu'>jaydon.devictoria@wsu.edu</a>"))
      )
    )
  )
)

server_raw <- function(input, output, session){
  observeEvent(input$go, {
    nm <- if (nzchar(input$name)) input$name else "Coug"
    output$greet <- renderUI(
      div(class = "greet-msg",
          span(paste("Hello,", nm, "👋")))
    )
  })
}

ui <- secure_app(ui_raw)

server <- function(input, output, session){
  res_auth <- secure_server(check_credentials = check_creds)
  server_raw(input, output, session)
}

shinyApp(ui, server)

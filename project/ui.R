#UI.R is where most of the site will be, at least for the early stages. Here we are simply setting up
#A simple,default look for the websites various UI elements, making them exist. 
#A good way to think of server and ui is as backend and frontend files. Server handles backend, UI frontend.

# here we make a theme object using bslib.
# Update to WSU-inspired branding and modern font.
theme <- bslib::bs_theme(
  version   = 5,
  base_font = bslib::font_google("Inter"),
  primary   = "#981E32",   # WSU crimson
  secondary = "#7a1828",
  bg        = "#f7f7f9",
  fg        = "#1f2937"
)

# MAIN UI SECTION
ui <- tagList(
  #this lets us add our style.css sheet to the rest of teh website through html tags.
  tags$head(
    # Load Inter font from Google for cleaner typography
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("console.log('Custom CSS loaded!');"))
  ),
  
  #creates a navigation bar that we can add too later. 
  navbarPage(
    title    = "Rate-My-Prof-WSU",
    id       = "menu",
    selected = "home",
    theme    = theme, 

    # Home tab
    tabPanel(
      title = "Home",
      value = "home",

      # Hero section
      div(class = "hero",
          div(class = "hero-content",
              h1("Find and share insights on WSU professors"),
              p(class = "subtext",
                "Search, rate, and review to help Cougs pick the right courses."),
              div(
                actionButton("loginB", "Register / Login", class = "btn btn-primary btn-lg")
              )
          )
      ),

      # Two cards below the hero
      fluidRow(
        column(
          width = 6,
          div(class = "card",
              h3("Top Professors"),
              p("Browse the highest-rated professors across departments.")
          )
        ),
        column(
          width = 6,
          div(class = "card",
              h3("Trending Courses"),
              p("See courses with the most recent ratings and reviews.")
          )
        )
      )
    ),


    tabPanel(
      title = "Reviews of the Day!",
      value = "reviewsOfTheDay",
      div(class = "card",
          h2("Today's Top Reviews"),
          p("Once data is available, the most engaged reviews will appear here.")
      )
    ),

    tabPanel(
      title = "Professors",
      value = "professors",
      div(class = "card",
          h2("Professors and their reviews"),
          p("This tab will list active professors and their reviews. (Coming soon)")
      )
    ),

    # About tab
    tabPanel(
      title = "About",
      value = "about",
      div(class = "card",
          h2("About Rate-My-Prof-WSU"),
          p("A community-driven tool for WSU students to find the right classes with confidence."),
          p("This project is not affiliated with Washington State University but aims to follow WSU-inspired branding.")
      )
    ),

    # Feedback tab
    tabPanel(
      title = "Feedback",
      value = "feedback",
      div(class = "card",
          h2("Feedback"),
          p("Have ideas or found a bug? Reach out!"),
          tags$ul(
            tags$li(HTML("Open an issue on GitHub: <a href='https://github.com/Bioticcc/Rate-My-Prof-WSU' target='_blank'>Repo</a>")),
            tags$li(HTML("Email: <a href='mailto:jaydon.devictoria@wsu.edu'>jaydon.devictoria@wsu.edu</a>"))
          )
      )
    )
  ), #end of navBarPage

  conditionalPanel(
  condition = "output.displayLogin",   
  tags$div( #we are adding this little tags$div to make this page act as an overlay on top of the site
    class = "overlay-root", #we use style.css to set this overlay-root's parameters
    tags$div(
      class = "overlay-card", #same with this
      tags$h2("Register / Login"),
      tags$p("Here you can register or log in using the shinyManager library!"),
      actionButton("backHome", "Back to Home", class = "btn btn-primary")
    )
  )
)


  
) #end of UI

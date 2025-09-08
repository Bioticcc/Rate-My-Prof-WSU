#ui.R


# ui.R

# setting a theme
theme <- bslib::bs_theme(
  version   = 5,
  bootswatch = "minty",
  base_font = bslib::font_google("Open Sans"),
  primary   = "#4A90E2",
  bg        = "#D3D3D3",
  fg        = "#2C3E50"
)

# MAIN UI SECTION
ui <- tagList(
  # load style.css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("console.log('Custom CSS loaded!');"))
  ),
  
  navbarPage(
    title    = "",
    id       = "menu",
    selected = "home",
    theme    = theme, 

    # Home tab
    tabPanel(
      title = "Home",
      value = "home",
      tags$div(
        class = "header-box",
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$h1("Rate My Prof (WSU)")
        )
      )
    ),

    # Authentication tab
    tabPanel(
      title = "Authentication",
      value = "auth",
      tags$div(
        class = "header-box",
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$h1("Submit WSU ID in order to publish reviews")
        )
      )
    )
  )
)

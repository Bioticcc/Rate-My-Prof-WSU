#UI.R is where most of the site will be, at least for the early stages. Here we are simply setting up
#A simple,default look for the websites various UI elements, making them exist. 
#A good way to think of server and ui is as backend and frontend files. Server handles backend, UI frontend.

#here we make a theme object, setting it with various variables using bslib to get a default theme.
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
  #this lets us add our style.css sheet to the rest of teh website through html tags.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("console.log('Custom CSS loaded!');"))
  ),
  
  #creates a navigation bar that we can add too later. 
  navbarPage(
    title    = "",
    id       = "menu",
    selected = "home",
    theme    = theme, 

    # Home tab
    tabPanel(
      title = "Home",
      value = "home", #this is how we connect to  buttons later on
      tags$div(
        class = "header-box", #NOTE: these classes are what we will assign  values to in style.css to change their look!
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$h1("Rate My Prof (WSU)")
        )
      ),
      tags$div(
        class = "subHeader-box",
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 10px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px",
          tags$h2(HTML("Using this webpage, you can find your upcoming professors and see their reviews,<br>star ratings, 
          and more!<br><br>If you have a current professor you would like to write a review for, please make an account or login to write a review!"))
        )
      ),
      actionButton( #here we are adding a button! we can then use server.R to connect it to specific actions by matchingthe inputID
        inputId = "loginB",
        label = "Register/Login",
        class = "btn"
       )
    ),

    tabPanel(
      title = "Reviews of the Day!",
      value = "reviewsOfTheDay",
      tags$div(
        class = "header-box", 
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$h1("Todays TOP reviews!") #once we get data collection up and running, this would be the reviews with the most clicks
        )
      ),
    ),

    tabPanel(
      title = "Professors",
      value = "professors",
      tags$div(
        class = "header-box", 
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$h1("Professors and their reviews!") #this will act as the main way to find reviews other then searching. Will list all current active profs, and their reviews
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

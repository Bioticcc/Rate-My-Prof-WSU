#Server is where we keep any functional aspects, like making reviews, publishing them,
#getting the list of WSU professors, authentication eventually, logging in, and more    


library(shinymanager)

server <- function(input, output, session) {

  # 1) Credentials checker 
  check_creds <- check_credentials(
    db         = "credentials.sqlite",
    passphrase = "change-this-passphrase" #still gotta change this
  )

  # 2) Start the login module
  auth <- callModule(
    shinymanager::auth_server,
    "login",
    check_credentials = check_creds
  )
  # auth_server() returns reactive values; auth$result becomes TRUE after login

  # 3) Control overlay visibility
  showLogin   <- reactiveVal(FALSE)
  auth_result <- reactive({ auth$result })

  observeEvent(input$loginB,   { showLogin(TRUE)  })   # open overlay when user clicks Login
  observeEvent(input$backHome, { showLogin(FALSE) })   # close overlay manually
  observeEvent(auth_result(), {
    if (isTRUE(auth_result())) showLogin(FALSE)        # auto-close overlay after successful login
  })

  # 4) ConditionalPanel driver: only show when overlay is open AND not authenticated
  output$displayLogin <- reactive({
    isTRUE(showLogin()) && !isTRUE(auth_result())
  })
  outputOptions(output, "displayLogin", suspendWhenHidden = FALSE)

}

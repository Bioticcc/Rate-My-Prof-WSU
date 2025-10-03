#Server is where we keep any functional aspects, like making reviews, publishing them,
#getting the list of WSU professors, authentication eventually, logging in, and more    


server <- function(input, output, session) {

    displayLogin <- reactiveVal(FALSE) #makes it a changable variable based on our button press! currently set to false
    observeEvent(input$loginB, { displayLogin(TRUE) }) #upon button click (remember button id is loginB) we set the var to true
    observeEvent(input$backHome, { displayLogin(FALSE) }) #upon button click (back to home) we set it to false again, to disable the page

    output$displayLogin <- reactive(displayLogin())
    outputOptions(output, "displayLogin", suspendWhenHidden = FALSE)

}

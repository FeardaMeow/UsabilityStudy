#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
  
library(shiny)
library(shinydashboard)
library(shinythemes)
library(googlesheets)

Logged = FALSE
my_username <- "test"
my_password <- "test"

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1VSSv36D8ngNDe9TAAtU0OLBQ2JoSiTFFleqa_Y3r6GA"
ss <- googlesheets::gs_key(sheet_key)

df.employee <- read.csv("fakeEmployee.csv")
Qlist <- read.csv("Qlist.csv")

tabs.content <- list(list(Title = "Tab1", Content = "Tab1 content"),
                     list(Title = "Tab2", Content = "Tab2 content"),
                     list(Title = "Tab3", Content = "Tab3 content"))

submenu.content <- list(menuSubItem("LL", tabName = "Situation LL"),
                        menuSubItem("LH", tabName = "Situation LH"),
                        menuSubItem("ML", tabName = "Situation ML"),
                        menuSubItem("MH", tabName = "Situation MH"),
                        menuSubItem("HL", tabName = "Situation HL"),
                        menuSubItem("HH", tabName = "Situation HH"))

menu.content <- list(menuItem("Landing Page", tabName = "lp"),
                     menuItem("Practice Situations", tabName = "ps"),
                     menuItem("Experiment Situations", tabName = "exp", sample(submenu.content)))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #################### SURVEY CODE #########################
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(Qlist))
  # Name each element of the vector based on the
  # second column of the Qlist
  names(results)  <<- Qlist[,2]  

  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0) 
      return(
        list(
          h5("Welcome to Shiny Survey Tool!")
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question")),
          radioButtons("survey", "Please Select:", 
                       c(option.list()))
        )
      )
    
    # Done screen
    if (input$Click.Counter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      )    
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Q", input$Click.Counter,":", 
      Qlist[input$Click.Counter,2]
    )
  })
  
  ################# LOGIN CODE #######################
  values <- reactiveValues(authenticated = FALSE)
  
# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    }
  })
  output$menuitem <- renderMenu({
    sidebarMenu(
      menu.content
    )
  })
}
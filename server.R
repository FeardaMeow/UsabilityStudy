library(shiny)
library(shinydashboard)
library(shinythemes)
library(googlesheets)
library(dplyr)

Logged = FALSE
my_username <- "test"
my_password <- "test"

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1VSSv36D8ngNDe9TAAtU0OLBQ2JoSiTFFleqa_Y3r6GA"
ss <- googlesheets::gs_key(sheet_key)

df.employee <- read.csv("fakeEmployee.csv")
Qlist <- read.csv("Qlist.csv")

submenu.content <- list(menuSubItem("Situation LL", tabName = "LL"),
                        menuSubItem("Situation LH", tabName = "LH"),
                        menuSubItem("Situation ML", tabName = "ML"),
                        menuSubItem("Situation MH", tabName = "MH"),
                        menuSubItem("Situation HL", tabName = "HL"),
                        menuSubItem("Situation HH", tabName = "HH"))

menu.content <- list(menuItem("Landing Page", tabName = "lp"),
                     menuItem("Practice Situations", tabName = "ps"),
                     menuItem("Experiment Situations", tabName = "exp", sample(submenu.content)))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #################### Low/High #########################
  output$LHtable <- renderUI( {
    dynamicUI.LH()
  })
  
  dynamicUI.LH <- reactive({
    # Initial scenario
    if (input$LHcounter==0) {
      output$LHdatatable <- renderDataTable(
        data.frame(df.employee %>% group_by(item_id, sequence_id, complete_qty, reject_qty) %>% 
                     summarise(predicted_hrs=mean(predicted_hrs)) %>% 
                     group_by(item_id, complete_qty, reject_qty) %>% summarise(predicted_hrs=round(sum(predicted_hrs), digits=1))),
        options = list(pageLength=10)
      )
      return(dataTableOutput("LHdatatable"))
    }
    
    # Survey
    if (input$LHcounter>0 & input$LHcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.LH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.LH()))
        )
      )
    
    # Done screen
    if (input$LHcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.LH <- reactive({
    qlist <- Qlist[input$LHcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.LH <- renderText({
    paste0(
      "Q", input$LHcounter,":", 
      Qlist[input$LHcounter,2]
    )
  })
  
  #################### High/High #########################
  output$HHtable <- renderUI( {
    dynamicUI.HH()
  })
  
  dynamicUI.HH <- reactive({
    # Initial scenario
    if (input$HHcounter==0) {
      output$HHdatatable <- renderDataTable(
        df.employee,
        options = list(searching=FALSE, pageLength=10)
      )
      return(dataTableOutput("HHdatatable"))
    }
    
    # Survey
    if (input$HHcounter>0 & input$HHcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.HH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.HH()))
        )
      )
    
    # Done screen
    if (input$HHcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.HH <- reactive({
    qlist <- Qlist[input$HHcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.HH <- renderText({
    paste0(
      "Q", input$HHcounter,":", 
      Qlist[input$HHcounter,2]
    )
  })
  
  #################### Medium/High #########################
  output$MHtable <- renderUI( {
    dynamicUI.MH()
  })
  
  dynamicUI.MH <- reactive({
    # Initial scenario
    if (input$MHcounter==0) {
      output$MHdatatable <- renderDataTable(
        df.employee,
        options = list(pageLength=10)
      )
      return(dataTableOutput("MHdatatable"))
    }
    
    # Survey
    if (input$MHcounter>0 & input$MHcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.MH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.MH()))
        )
      )
    
    # Done screen
    if (input$MHcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.MH <- reactive({
    qlist <- Qlist[input$MHcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.MH <- renderText({
    paste0(
      "Q", input$MHcounter,":", 
      Qlist[input$MHcounter,2]
    )
  })
  
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
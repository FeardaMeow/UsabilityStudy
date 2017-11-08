library(shiny)
library(shinydashboard)
library(googlesheets)
library(dplyr)
library(plotly)
library(DT)

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
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$LHcounter==0) {
      output$LHdatatable <- renderDataTable(
        data.frame(df.employee %>% group_by(item_id, sequence_id, complete_qty, reject_qty) %>% 
                     summarise(predicted_hrs=mean(predicted_hrs)) %>% 
                     group_by(item_id, complete_qty, reject_qty) %>% summarise(predicted_hrs=round(sum(predicted_hrs), digits=1))),
        options = list(pageLength=10)
      )
      return(list(fluidRow(box(width=12,
                               h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                                    "with quantity", as.character(testQ$complete_qty), sep=" ")})))),
                  fluidRow(
                    box(width=12,
                        div(style = 'overflow-x: scroll', DT::dataTableOutput("LHdatatable"))
                        )
                    ),
                  fluidRow(
                    box(
                      numericInput("LHanswer", "Answer:", value=0, min=0, max=100, step=0.1)
                    )#End Box
                  )#End Fluid Row
              )
        )
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
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$HHcounter==0) {
      output$HHdatatable <- renderDataTable(
        df.employee,
        options = list(searching=FALSE, pageLength=10)
      )
      return(list(fluidRow(box(width=12,
                               h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                                    "with quantity", as.character(testQ$complete_qty), sep=" ")})))),
                  fluidRow(
                    box(width=12,
                        div(style = 'overflow-x: scroll', DT::dataTableOutput("HHdatatable"))
                        )
                           ),
                  fluidRow(
                    box(
                      numericInput("HHanswer", "Answer:", value=0, min=0, max=100, step=0.1)
                    )#End Box
                  )#End Fluid Row
                  )
        )
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
    testQ <- sample_n(df.employee,1)
    
    # Initial scenario
    if (input$MHcounter==0) {
      output$MHdatatable <- renderDataTable(
        df.employee,
        options = list(pageLength=10)
      )
      return(list(
          fluidRow(box(width=12,
                                 h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                                   "with quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(width=12,
                div(style = 'overflow-x: scroll', DT::dataTableOutput("MHdatatable"))
                )
                   ),
          fluidRow(
            box(
              numericInput("MHanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
        )
      )
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
  
  ################### LOW/LOW ##############################
  output$LLview <- renderUI( {
    dynamicUI.LL()
  })
  
  dynamicUI.LL <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$LLcounter==0) {
      
      output$LLprediction <- renderValueBox({
        pred <- round(log2(as.numeric(input$item_id_LL) * input$complete_qty_LL), 3)
        valueBox(
          paste0(pred), "Predicted Hours", icon = icon("list"),
          color = "purple"
        )
      }
      )
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                            "with quantity", as.character(testQ$complete_qty), sep=" ")})))
                   ), #End Fluid Row
          fluidRow(
          box(
            selectInput(inputId = "item_id_LL", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
            numericInput(inputId = "complete_qty_LL", label = "Quantity:", value = 1, min = 1, max = 100, step = 1)
          ),
          valueBoxOutput("LLprediction")
          ), #End Fluid Row
          fluidRow(
            box(
              numericInput("LLanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
        )
      )
      
    }
    
    # Survey
    if (input$LLcounter>0 & input$LLcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.LL")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.LL()))
        )
      )
    
    # Done screen
    if (input$LLcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.LL <- reactive({
    qlist <- Qlist[input$LLcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.LL <- renderText({
    paste0(
      "Q", input$LLcounter,":", 
      Qlist[input$LLcounter,2]
    )
  })
  ################## Median/Low ###########################
  output$MLview <- renderUI( {
    dynamicUI.ML()
  })
  
  dynamicUI.ML <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$MLcounter==0) {
      output$MLprediction <- renderValueBox({
        pred <- round(log2(as.numeric(input$item_id_ML) * input$complete_qty_ML), 3)
        valueBox(
          paste0(pred), "Predicted Hours", icon = icon("list"),
          color = "purple"
        )
        }
      )
      
      output$item_id_hist_ML <- renderPlotly({
        item_id <- as.character(df.employee$item_id)
        plot_ly(x = item_id, type = "histogram") %>%
          layout(xaxis= list(title = "Item ID"),
                 yaxis = list(title = 'Number of unfinished process'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      output$box_ML <- renderPlotly({
        plot_ly(y = ~df.employee$complete_qty, type = "box", name = "Complete") %>%
          add_trace(y = ~df.employee$reject_qty, name = 'Reject') %>%
          layout(yaxis = list(title = "Quantity"))
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                            "with quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(plotlyOutput("item_id_hist_ML")),
            box(plotlyOutput("box_ML"))
          ),
          fluidRow(box(
            selectInput(inputId = "item_id_ML", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
            numericInput(inputId = "complete_qty_ML", label = "Complete Quantity:", value = 1, min = 1, max = 100, step = 1)
          ),
          valueBoxOutput("MLprediction")),
          fluidRow(
            box(
              numericInput("MLanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
          
        )
      )
      
    }
    
    # Survey
    if (input$MLcounter>0 & input$MLcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.ML")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.ML()))
        )
      )
    
    # Done screen
    if (input$MLcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.ML <- reactive({
    qlist <- Qlist[input$MLcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.ML <- renderText({
    paste0(
      "Q", input$MLcounter,":", 
      Qlist[input$MLcounter,2]
    )
  })
  ################## High/Low ###########################
  output$HLview <- renderUI( {
    dynamicUI.HL()
  })
  
  dynamicUI.HL <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$HLcounter==0) {
      output$HLprediction <- renderValueBox({
        pred <- round(log2(as.numeric(input$item_id_HL) * input$complete_qty_HL), 3)
        valueBox(
          paste0(pred), "Predicted Hours", icon = icon("list"),
          color = "purple"
        )
      }
      )
      
      output$item_id_hist_HL <- renderPlotly({
        item_id <- as.character(df.employee$item_id)
        plot_ly(x = item_id, type = "histogram") %>%
          layout(xaxis= list(title = "Item ID"),
                 yaxis = list(title = 'Number of unfinished process'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      output$box_HL <- renderPlotly({
        plot_ly(y = ~df.employee$complete_qty, type = "box", name = "Complete") %>%
          add_trace(y = ~df.employee$reject_qty, name = 'Reject') %>%
          layout(yaxis = list(title = "Quantity"), showlegend = F)
      })
      
      output$mo_id_bar_HL <- renderPlotly({
        df.mo_id <- as.data.frame(table(df.employee$mo_id))
        colnames(df.mo_id) <- c("mo.id", "num")
        plot_ly(df.mo_id) %>%
          add_trace(x = ~mo.id, y = ~num, type = 'bar') %>% 
          layout(xaxis= list(title = "mo_id"),
                 yaxis = list(title = 'Number'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      output$sequence_id_bar_HL <- renderPlotly({
        df.sequence_id <- as.data.frame(table(df.employee$sequence_id))
        colnames(df.sequence_id) <- c("sequence_id", "num")
        plot_ly(df.sequence_id) %>%
          add_trace(x = ~sequence_id, y = ~num, type = 'bar') %>% 
          layout(xaxis= list(title = "Sequence ID"),
                 yaxis = list(title = 'Number'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      output$eid_hist_HL <- renderPlotly({
        eid <- as.character(df.employee$eid) 
        plot_ly(x = eid, type = "histogram") %>%
          layout(xaxis= list(title = "Employee ID"),
                 yaxis = list(title = 'Number of jobs to finish'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      output$mo_des_his_HL <- renderPlotly({
        plot_ly(x = df.employee$mo_description, type = "histogram") %>%
          layout(xaxis= list(title = "MO description", tickangle = -45),
                 yaxis = list(title = 'Frequency'),
                 dragmode = "select", showlegend = FALSE)
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Please estimate the completion time for an order of item id",as.character(testQ$item_id),
                                            "with quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(plotlyOutput("item_id_hist_HL"), width = 3),
            box(plotlyOutput("box_HL"), width = 3),
            box(plotlyOutput("mo_id_bar_HL"), width = 3),
            box(plotlyOutput("sequence_id_bar_HL"), width = 3)
          ),
          fluidRow(
            box(plotlyOutput("eid_hist_HL"), width = 5),
            box(plotlyOutput("mo_des_his_HL"), width = 4),
            box(width = 3,
            selectInput(inputId = "item_id_HL", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
            numericInput(inputId = "complete_qty_HL", label = "Complete Quantity:", value = 1, min = 1, max = 100, step = 1),
            valueBoxOutput("HLprediction", width = 2.5)
            )
          ),
          fluidRow(
            box(
              numericInput("HLanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
        )
      )
      
    }
    
    # Survey
    if (input$HLcounter>0 & input$HLcounter<=nrow(Qlist))  
      return(
        list(
          h5(textOutput("question.HL")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.HL()))
        )
      )
    
    # Done screen
    if (input$HLcounter>nrow(Qlist))
      return(
        list(
          h4("DONE")
        )
      ) 
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.HL <- reactive({
    qlist <- Qlist[input$HLcounter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.HL <- renderText({
    paste0(
      "Q", input$HLcounter,":", 
      Qlist[input$HLcounter,2]
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
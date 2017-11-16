library(shiny)
library(shinydashboard)
library(googlesheets)
library(dplyr)
library(plotly)
library(DT)
library(shinyjs)

Logged = FALSE
my_username <- "test"
my_password <- "test"

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1VSSv36D8ngNDe9TAAtU0OLBQ2JoSiTFFleqa_Y3r6GA"
ss <- googlesheets::gs_key(sheet_key)

df.employee <- read.csv("fakeEmployee.csv")
df.group <- data.frame(df.employee %>% group_by(item_id, sequence_id, complete_qty, reject_qty) %>% 
                         summarise(Lower_bound=mean(Lower_bound), predicted_hrs=mean(predicted_hrs), Upper_bound=mean(Upper_bound)) %>% 
                         group_by(item_id, complete_qty, reject_qty) %>% summarise(Lower_bound=round(sum(Lower_bound), digits=1),
                                                                                   predicted_hrs=round(sum(predicted_hrs), digits=1),
                                                                                   Upper_bound=round(sum(Upper_bound), digits=1)))
df.plot <- data.frame(
  df.group %>%
    group_by(item_id, complete_qty) %>%
    summarise(Lower_bound=round(sum(Lower_bound), digits=1),
              predicted_hrs=round(sum(predicted_hrs), digits=1),
              Upper_bound=round(sum(Upper_bound), digits=1))
)
df.plot$item_id <- as.factor(df.plot$item_id)
df.plot$complete_qty <- as.factor(df.plot$complete_qty)
Qlist <- read.csv("Qlist.csv")

submenu.content <- list("LL" = menuSubItem("Scenario LL", tabName = "LL"),
                        "LH" = menuSubItem("Scenario LH", tabName = "LH"),
                        "ML" = menuSubItem("Scenario ML", tabName = "ML"),
                        "MH" = menuSubItem("Scenario MH", tabName = "MH"),
                        "HL" = menuSubItem("Scenario HL", tabName = "HL"),
                        "HH" = menuSubItem("Scenario HH", tabName = "HH"))

randomsubtab <- sample(submenu.content)

menu.content <- list(menuItem("Landing Page", tabName = "lp"),
                     menuItem("Practice Scenarios", tabName = "ps"),
                     menuItem("Usability Study Scenarios", tabName = "exp", randomsubtab))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #################### Landing Page #########################
  output$LandingPage <- renderUI( {
    dynamicUI.LP()
  }) #End RenderUI
  
  dynamicUI.LP <- reactive({
    if(input$sigSubmit==0) {
      return(
        list(
          withTags({
            div(style="overflow-y: scroll;height:80vh;",
                h1("DMDII - Visibility Tool Usability Study", align="center"),
                p("We are asking you to be in a research study. The purpose of this consent form is to give you the information you will need to help you decide whether to be in the study or not. Please read the form carefully. You may ask questions about the purpose of the research by emailing Steven Hwang at hwang216@uw.edu, what we would ask you to do, the possible risks and benefits, your rights as a volunteer, and anything else about the research or this form that is not clear. When we have answered all your questions, you can decide if you want to be in the study or not. This process is called informed consent you may screen shot this page for your records."),
                h2("Purpose of the Study", align="center"),
                p("The objective of the current study is to assess the users' use and acceptance of the visibility tool assuming that the real-time information is accurate and reliable. This research will help to make sure that user interface is designed to maximize the understanding and use of the part flow information as well as the overall system usefulness."),
                h2("Study Procedures", align="center"),
                p("Approximately 10 people will take part in this study remotely at their respective work location. Your involvement in this study will consist of using an web app for approximately 30-40 minutes. There may be additional contact from the study team for a follow up study."),
                p("Your written consent will be obtained upon using the web app. You may email us any questions you have."),
                p("When using the usability web app, you will be asked to complete a questionnaire that covers some general questions relating to your current position and demographics. For example, the questions will ask you how much experience do you have at your current role? After every scenario, you will also take a short questionnaire about aspects of the UI and information displayed."),
                p("You will receive instructions on how to operate the visibility tool and you will be asked to complete a practice scenario that is about 5 minutes long, so that you can become familiar with the visibility tool."),
                p("The study will follow this same procedure, which are as follows:"),
                ul(
                  li("Read informed consent"),
                  li("Practice navigating the visbility tool"),
                  li("Practice scenario for visibility tool"),
                  li("Visibility tool questionnaire"),
                  li("Repeat for all scenarios")
                ),
                p("You may skip any questions that you do not wish to answer on the surveys. All evaluation studies will be screen captured. The system contains a screen capture software that will capture all the interactions of the study subject and the visibility tool."),
                h2("RISKS, STRESS, OR DISCOMFORT", align="center"),
                h2("DMDII - Visibility Tool Evaluation", align="center"),
                p("There may be some risks from being in this study. Despite limiting the study to a short period of time, you may feel discomfort associated with eye soreness and dryness associated with extended computer use. These feelings were usually mild and consisted of slight dryness and soreness of the eyes. These effects typically last for only a short time, usually 10-15 minutes, after leaving the visibility tool. If you become uncomfortable at any time during your participation, please inform the experimenter immediately."),
                p("In case of a breach of confidentially, your survey responses and screen capture data could be accessed. To minimize this impact, your name will not be directly associated with this data, and the data will be securely stored in a password protected computer in a limited access room."),
                h2("BENEFITS OF THE STUDY", align="center"),
                p("There are no direct benefits to participants in the study. However, we hope that information from this study will help us to evaluate the effects of the visibility tool and the presentation of the data in decision making. Feedback provided by the participants will help to shape the design of future iterations of the visibility tool."),
                h2("CONFIDENTIALITY OF RESEARCH INFORMATION", align="center"),
                p("Your confidentiality will be protected throughout data collection and analysis. This will be accomplished by assigning a study number to you, for which your data will be identified by. All the data collected from this study will be represented by this number; your name will not be directly associated with your questionnaire answers, screen capture data or survey data, but will be linked through by a study code. This link between you as a subject and the study number will be maintained until the study is complete and all records retention requirements have passed, at which point it will be destroyed."),
                p("The experimental results, and survey results will be kept on a password secure device and access will be given only to this research team at University of Washington."),
                p("Government or university staff sometimes review studies such as this one to make sure they are being done safely and legally.  If a review of this study takes place, your records may be examined. The reviewers will protect your privacy. The study records will not be used to put you at legal risk of harm."),
                p("We will keep your participation in this research study confidential to the extent permitted by law.  To help protect your confidentiality, you will be assigned a study number that will be used instead of your name to identify all data collected for the study. All information you provide will be confidential. However, if we learn that you intend to harm yourself or others, we must report that to the authorities."),
                h2("OTHER INFORMATION", align="center"),
                p("You may refuse to participate, and you are free to withdraw from this study at any time without penalty or loss of benefits to which you are otherwise entitled. The data collected from the participant surveys will be collectively analyzed. The findings from this research may be publicly released in final reports or other publications for scientific, educational, outreach, or research purposes."),
                p("Your participation in this study is entirely voluntary, and you are free to withdraw from the study at any time."),
                h2("RESEARCH-RELATED INJURY", align="center"),
                p("If you think you have a medical problem or illness related to this research, contact study staff Steven Hwang at hwang216@uw.edu."),
                h3("Subject's statement"),
                p("This study has been explained to me.  I volunteer to take part in this research.  I have had a chance to ask questions.  If I have questions later about the research, or if I have been harmed by participating in this study, I can contact one of the researchers listed on the first page of this consent form.  If I have questions about my rights as a research subject, I can call the Human Subjects Division at (206) 543-0098 or call collect at (206) 221-5940.")
            )#End Div
          }),
          textInput("signature", "Signature:")
        ) #End List
      ) #End return
    } #End If
    if (input$sigSubmit == 1) {
      updateActionButton(session, "sigSubmit", label = 'Start Practice')
      return(
        list(
          withTags({
            div(style="overflow-y: scroll;height:80vh;",
              h1("Instructions"),
              p("Please read all instructions carefully before continuing. The usability study consists of practice sessions and six usability study scenarios. 
                The practice sessions will familiarize you with the dashboard and presented data, and explain how to answer the questions for each scenario. 
                Immediately after the practice session, the usability scenarios will be presented to you. 
                In each scenario you will be given 3 questions and asked to estimate the completion time for an specified item and 
                quantity using the provided dashboard."), 
              p("The data shown in each scenario will include completion time for each process. For example, item id 1 might have 3 processes that need to happen before the item
                is complete. The 3 processes could include drilling, boring, and washing. To item completion time will be a combination of the prediction times for each of the processes (drilling+boring+washing) together for the same quantity amount.
                This will be covered more in depth in the practice scenario."), 
              p("The instructions for the usability study are:"),
              ol(
                li("Click on the triple line button to reveal the sidebar menu"),
                li("Click on 'Practice Scenarios' menu item in the sidebar menu"),
                li("Click on 'Usability Study Scenarios' to reveal the usability study scenarios that should be completed"),
                li("Note the order that the usability study scenarios, this is the order that they must be completed in. Please do not skip around when completing scenarios"),
                li("In each scenario, you will be asked to answer 3 questions using that provided dashboard. Input your answer to the questions in the provided text box and press submit to move onto the next question."),
                li("After answering the 3 questions, you will be then asked to assess the usability and trust of the dashboard by answering a questionnaire"),
                li("Repeat 5-6 till all scenarios are complete")
              )
            ) #End div
          }) #End withTags
#          shinyjs::hide("sigSubmit")
        ) #End list
      ) #End return
    } #End else
    if (input$sigSubmit >= 2){
      return(
        list(
          updateTabItems(session, "tabs", "ps")
        )
      )
    }
  }) #End reactive
  
  #################### Practice Scenarios###################
  output$PSview <- renderUI( {
    dynamicUI.PS()
  })
  
  dynamicUI.PS <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$PScounter == 0) {
      
      output$PSprediction <- renderValueBox({
        pred <- df.plot$predicted_hrs[df.plot$item_id == input$item_id_PS & df.plot$complete_qty == input$complete_qty_PS]
        valueBox(
          paste0(ifelse(identical(pred, numeric(0)), NA, pred)), "Median Predicted Hours", icon = icon("list"),
          color = "purple")
      })
      
      output$PSupper <- renderValueBox({
        upper <- df.plot$Upper_bound[df.plot$item_id == input$item_id_PS & df.plot$complete_qty == input$complete_qty_PS]
        valueBox(
          paste0(ifelse(identical(upper, numeric(0)), NA, upper)), "75% Upper Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$PSlower <- renderValueBox({
        lower <- df.plot$Lower_bound[df.plot$item_id == input$item_id_PS & df.plot$complete_qty == input$complete_qty_PS]
        valueBox(
          paste0(ifelse(identical(lower, numeric(0)), NA, lower)), "25% Lower Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Question", input$PScounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                            "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))
          ), #End Fluid Row
          fluidRow(
            box(width = 3,
                selectInput(inputId = "item_id_PS", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
                numericInput(inputId = "complete_qty_PS", label = "Quantity:", value = 1, min = 1, max = 100, step = 1)
            ),
            box(width = 9,
                valueBoxOutput("PSupper"),
                valueBoxOutput("PSprediction"),
                valueBoxOutput("PSlower")
            )
          ), #End Fluid Row
          fluidRow(
            box(
              numericInput("PSanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
        )
      )}
    
    if (input$PScounter == 1){
      colnames(df.group) <- c("Item ID", "Complete Quantity", "Reject Quantity", "25% Lower Prediction Bound", "Median Predicted Hours", "75% Upper Prediction Bound")
      output$PSdatatable <- renderDataTable(
        df.group,
        options = list(pageLength=10)
      )
      return(list(fluidRow(box(width=12,
                               h1(renderText({paste("Question", input$PScounter+1, ":", "Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                                    "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
                  fluidRow(
                    box(width=12,
                        div(style = 'overflow-x: scroll', DT::dataTableOutput("PSdatatable"))
                    )
                  ),
                  fluidRow(
                    box(
                      numericInput("PSanswer", "Answer:", value=0, min=0, max=100, step=0.1)
                    )#End Box
                  )#End Fluid Row
      )
      )
    }
    
    if (input$PScounter == 2){
      output$heat_PS <- renderPlotly({
        plot_ly(df.plot, x = ~item_id, y = ~complete_qty, z = ~predicted_hrs, type = "heatmap", colorscale = "Greys", hoverinfo = 'text', colorbar = list(title = "Predicted Hours"),
                text = ~paste0('Item ID: ', item_id, '\n', 
                               'Complete Quantity: ', complete_qty, '\n',
                               '75% Upper Prediction Bound: ', Upper_bound, '\n',
                               'Median Predicted Hours: ', predicted_hrs, '\n',
                               '25% Lower Prediction Bound: ', Lower_bound)) %>%
          layout(xaxis= list(title = "Item ID"),
                 yaxis = list(title = 'Complete Quantity'))
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Question", input$PScounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                            "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(plotlyOutput("heat_PS"),width = 12)
          ),
          fluidRow(
            box(
              numericInput("PSanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
          
        )
      )
    }
    
    # Done screen
    if (input$PScounter == 3){
      updateActionButton(session, 'PScounter', label = 'Start!')
      return(
        list(
          h4("You have finished the practive scenarios. Please start the usability study scenarios when you are ready!")        
          )
      )
    }
    
    if (input$PScounter >= 4){
      return(
        list(
          updateTabItems(session, "tabs", names(randomsubtab)[1])
        )
      )
    }
      
  })
  
  
  
  #################### Low/High #########################
  output$LHtable <- renderUI( {
    dynamicUI.LH()
  })
  
  dynamicUI.LH <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$LHcounter<3) {
      colnames(df.group) <- c("Item ID", "Complete Quantity", "Reject Quantity", "25% Lower Prediction Bound", "Median Predicted Hours", "75% Upper Prediction Bound")
      output$LHdatatable <- renderDataTable(
        df.group,
        options = list(pageLength=10)
      )
      return(list(fluidRow(box(width=12,
                               h1(renderText({paste("Question", input$LHcounter+1, ":", "Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                                    "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
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
    if (input$LHcounter>=3 & input$LHcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.LH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.LH()))
        )
      )
    if (input$LHcounter == nrow(Qlist)+3){
      updateActionButton(session, 'LHcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$LHcounter > nrow(Qlist)+3){
      if (which(names(randomsubtab) == 'LH') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
        list(
          updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'LH') + 1])
        )
      )
      }
    }
      
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.LH <- reactive({
    qlist <- Qlist[input$LHcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.LH <- renderText({
    paste0(
      "Q", input$LHcounter-2,":", 
      Qlist[input$LHcounter-2,2]
    )
  })
  
  #################### High/High #########################
  output$HHtable <- renderUI( {
    dynamicUI.HH()
  })
  
  dynamicUI.HH <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$HHcounter<3) {
      colnames(df.employee) <- c('Employee ID', 'Last Name', 'First Name', 'MO ID', 'MO Description', 'Item ID', 'Sequence ID', 'Sequence Description',
                                 'Complete Quantity', 'Reject Quantity', 'Start Date', 'Finish Date', '25% Lower Predciiton Bound', 'Median Predited Hours',
                                 '75% Upper Prediciton Bound')
      output$HHdatatable <- renderDataTable(
        df.employee,
        options = list(searching=FALSE, pageLength=10)
      )
      return(list(fluidRow(box(width=12,
                               h1(renderText({paste("Question", input$HHcounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                                    "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
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
    if (input$HHcounter>=3 & input$HHcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.HH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.HH()))
        )
      )
    
    if (input$HHcounter == nrow(Qlist)+3){
      updateActionButton(session, 'HHcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$HHcounter>nrow(Qlist)+2){
      if (which(names(randomsubtab) == 'HH') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
          list(
            updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'HH') + 1])
          )
        )
      }
    }
    
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.HH <- reactive({
    qlist <- Qlist[input$HHcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.HH <- renderText({
    paste0(
      "Q", input$HHcounter-2,":", 
      Qlist[input$HHcounter-2,2]
    )
  })
  
  #################### Medium/High #########################
  output$MHtable <- renderUI( {
    dynamicUI.MH()
  })
  
  dynamicUI.MH <- reactive({
    testQ <- sample_n(df.employee,1)
    
    # Initial scenario
    if (input$MHcounter<3) {
      colnames(df.employee) <- c('Employee ID', 'Last Name', 'First Name', 'MO ID', 'MO Description', 'Item ID', 'Sequence ID', 'Sequence Description',
                                 'Complete Quantity', 'Reject Quantity', 'Start Date', 'Finish Date', '25% Lower Predciiton Bound', 'Median Predited Hours',
                                 '75% Upper Prediciton Bound')
      output$MHdatatable <- renderDataTable(
        df.employee,
        options = list(pageLength=10)
      )
      return(list(
          fluidRow(box(width=12,
                                 h1(renderText({paste("Question", input$MHcounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                                   "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
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
    if (input$MHcounter>=3 & input$MHcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.MH")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.MH()))
        )
      )
    
    if (input$MHcounter == nrow(Qlist)+3){
      updateActionButton(session, 'MHcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$MHcounter>nrow(Qlist)+2){
      if (which(names(randomsubtab) == 'MH') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
          list(
            updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'MH') + 1])
          )
        )
      }
    }
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.MH <- reactive({
    qlist <- Qlist[input$MHcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.MH <- renderText({
    paste0(
      "Q", input$MHcounter-2,":", 
      Qlist[input$MHcounter-2,2]
    )
  })
  
  ################### LOW/LOW ##############################
  output$LLview <- renderUI( {
    dynamicUI.LL()
  })
  
  dynamicUI.LL <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$LLcounter<3) {
      
      output$LLprediction <- renderValueBox({
        pred <- df.plot$predicted_hrs[df.plot$item_id == input$item_id_LL & df.plot$complete_qty == input$complete_qty_LL]
        valueBox(
          paste0(ifelse(identical(pred, numeric(0)), NA, pred)), "Median Predicted Hours", icon = icon("list"),
          color = "purple")
      })
      
      output$LLupper <- renderValueBox({
        upper <- df.plot$Upper_bound[df.plot$item_id == input$item_id_LL & df.plot$complete_qty == input$complete_qty_LL]
        valueBox(
          paste0(ifelse(identical(upper, numeric(0)), NA, upper)), "75% Upper Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$LLlower <- renderValueBox({
        lower <- df.plot$Lower_bound[df.plot$item_id == input$item_id_LL & df.plot$complete_qty == input$complete_qty_LL]
        valueBox(
          paste0(ifelse(identical(lower, numeric(0)), NA, lower)), "25% Lower Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Question", input$LLcounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                            "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))
                   ), #End Fluid Row
          fluidRow(
          box(width = 3,
            selectInput(inputId = "item_id_LL", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
            numericInput(inputId = "complete_qty_LL", label = "Quantity:", value = 1, min = 1, max = 100, step = 1)
          ),
          box(width = 9,
            valueBoxOutput("LLupper"),
            valueBoxOutput("LLprediction"),
            valueBoxOutput("LLlower")
          )
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
    if (input$LLcounter>=3 & input$LLcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.LL")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.LL()))
        )
      )
    
    if (input$LLcounter == nrow(Qlist)+3){
      updateActionButton(session, 'LLcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$LLcounter>nrow(Qlist)+2){
      if (which(names(randomsubtab) == 'LL') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
          list(
            updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'LL') + 1])
          )
        )
      }
    }
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.LL <- reactive({
    qlist <- Qlist[input$LLcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.LL <- renderText({
    paste0(
      "Q", input$LLcounter-2,":", 
      Qlist[input$LLcounter-2,2]
    )
  })
  ################## Median/Low ###########################
  output$MLview <- renderUI( {
    dynamicUI.ML()
  })
  
  dynamicUI.ML <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$MLcounter<3) {
      output$MLprediction <- renderValueBox({
        pred <- df.plot$predicted_hrs[df.plot$item_id == input$item_id_ML & df.plot$complete_qty == input$complete_qty_ML]
        valueBox(
          paste0(ifelse(identical(pred, numeric(0)), NA, pred)), "Median Predicted Hours", icon = icon("list"),
          color = "purple")
      })
      
      output$MLupper <- renderValueBox({
        upper <- df.plot$Upper_bound[df.plot$item_id == input$item_id_ML & df.plot$complete_qty == input$complete_qty_ML]
        valueBox(
          paste0(ifelse(identical(upper, numeric(0)), NA, upper)), "75% Upper Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$MLlower <- renderValueBox({
        lower <- df.plot$Lower_bound[df.plot$item_id == input$item_id_ML & df.plot$complete_qty == input$complete_qty_ML]
        valueBox(
          paste0(ifelse(identical(lower, numeric(0)), NA, lower)), "25% Lower Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$heat_ML <- renderPlotly({
        plot_ly(df.plot, x = ~item_id, y = ~complete_qty, z = ~predicted_hrs, type = "heatmap", colorscale = "Greys", hoverinfo = 'text', colorbar = list(title = "Predicted Hours"),
                text = ~paste0('Item ID: ', item_id, '\n', 
                              'Complete Quantity: ', complete_qty, '\n',
                              '75% Upper Prediction Bound: ', Upper_bound, '\n',
                              'Median Predicted Hours: ', predicted_hrs, '\n',
                              '25% Lower Prediction Bound: ', Lower_bound)) %>%
          layout(xaxis= list(title = "Item ID"),
                 yaxis = list(title = 'Complete Quantity'))
      })
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Question", input$MLcounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                            "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(plotlyOutput("heat_ML"),width = 12)
          ),
          fluidRow(
            box(width = 3,
                selectInput(inputId = "item_id_ML", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
                numericInput(inputId = "complete_qty_ML", label = "Complete Quantity:", value = 1, min = 1, max = 100, step = 1)
            ),
            box(width = 9,
              valueBoxOutput("MLupper"),
              valueBoxOutput("MLprediction"),
              valueBoxOutput("MLlower")
            )
          ),
          fluidRow(
            box(
              numericInput("MLanswer", "Answer:", value=0, min=0, max=100, step=0.1)
            )#End Box
          )#End Fluid Row
          
        )
      )
      
    }
    
    # Survey
    if (input$MLcounter>=3 & input$MLcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.ML")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.ML()))
        )
      )
    
    if (input$MLcounter == nrow(Qlist)+3){
      updateActionButton(session, 'MLcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$MLcounter>nrow(Qlist)+2){
      if (which(names(randomsubtab) == 'ML') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
          list(
            updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'ML') + 1])
          )
        )
      }
    }
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.ML <- reactive({
    qlist <- Qlist[input$MLcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.ML <- renderText({
    paste0(
      "Q", input$MLcounter-2,":", 
      Qlist[input$MLcounter-2,2]
    )
  })
  ################## High/Low ###########################
  output$HLview <- renderUI( {
    dynamicUI.HL()
  })
  
  dynamicUI.HL <- reactive({
    testQ <- sample_n(df.employee,1)
    # Initial scenario
    if (input$HLcounter<3) {
      output$HLprediction <- renderValueBox({
        pred <- df.plot$predicted_hrs[df.plot$item_id == input$item_id_HL & df.plot$complete_qty == input$complete_qty_HL]
        valueBox(
          paste0(ifelse(identical(pred, numeric(0)), NA, pred)), "Median Predicted Hours", icon = icon("list"),
          color = "purple")
      })
      
      output$HLupper <- renderValueBox({
        upper <- df.plot$Upper_bound[df.plot$item_id == input$item_id_HL & df.plot$complete_qty == input$complete_qty_HL]
        valueBox(
          paste0(ifelse(identical(upper, numeric(0)), NA, upper)), "75% Upper Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$HLlower <- renderValueBox({
        lower <- df.plot$Lower_bound[df.plot$item_id == input$item_id_HL & df.plot$complete_qty == input$complete_qty_HL]
        valueBox(
          paste0(ifelse(identical(lower, numeric(0)), NA, lower)), "25% Lower Prediction Bound", icon = icon("list"),
          color = "purple")
      })
      
      output$heat_HL <- renderPlotly({
        plot_ly(df.plot, x = ~item_id, y = ~complete_qty, z = ~predicted_hrs, type = "heatmap", colorscale = "Greys", hoverinfo = 'text', colorbar = list(title = "Predicted Hours"),
                text = ~paste0('Item ID: ', item_id, '\n', 
                               'Complete Quantity: ', complete_qty, '\n',
                               '75% Upper Prediction Bound: ', Upper_bound, '\n',
                               'Median Predicted Hours: ', predicted_hrs, '\n',
                               '25% Lower Prediction Bound: ', Lower_bound)) %>%
          layout(xaxis= list(title = "Item ID"),
                 yaxis = list(title = 'Complete Quantity'))
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
      
      return(
        list(
          fluidRow(box(width=12,
                       h1(renderText({paste("Question", input$HLcounter+1, ":","Please estimate the completion time for an order of Item ID",as.character(testQ$item_id),
                                            "with Complete Quantity", as.character(testQ$complete_qty), sep=" ")})))),
          fluidRow(
            box(plotlyOutput("box_HL"), width = 4),
            box(plotlyOutput("mo_id_bar_HL"), width = 4),
            box(plotlyOutput("sequence_id_bar_HL"), width = 4)
          ),
          fluidRow(
            box(plotlyOutput('heat_HL'), width = 12)
          ),
          fluidRow(
            box(plotlyOutput("eid_hist_HL"), width = 6),
            box(width = 2,
            selectInput(inputId = "item_id_HL", label = "Item ID to predict:", choices = sort(unique(df.employee$item_id))),
            numericInput(inputId = "complete_qty_HL", label = "Complete Quantity:", value = 1, min = 1, max = 100, step = 1)
            ),
            box(width = 4,
              valueBoxOutput("HLupper", width = 12),
              valueBoxOutput("HLprediction", width = 12),
              valueBoxOutput("HLlower", width = 12)
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
    if (input$HLcounter>=3 & input$HLcounter<=nrow(Qlist)+2)  
      return(
        list(
          h5(textOutput("question.HL")),
          radioButtons("survey", "Please Select:", 
                       c(option.list.HL()))
        )
      )
    
    if (input$HLcounter == nrow(Qlist)+3){
      updateActionButton(session, 'HLcounter', label = 'Next')
      return(
        list(
          h4('You have finished the curren scenario. Please click on the Next button to go to the next scenario.')
        )
      )
    }
    
    # Done screen
    if (input$HLcounter>nrow(Qlist)+2){
      if (which(names(randomsubtab) == 'HL') == 6){
        return(
          list(
            h4("Congratulations! Thank you for finishing the usability study!")
          )
        )
      } else {
        return(
          list(
            updateTabItems(session, "tabs", names(randomsubtab)[which(names(randomsubtab) == 'HL') + 1])
          )
        )
      }
    }
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list.HL <- reactive({
    qlist <- Qlist[input$HLcounter-2,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question.HL <- renderText({
    paste0(
      "Q", input$HLcounter-2,":", 
      Qlist[input$HLcounter-2,2]
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
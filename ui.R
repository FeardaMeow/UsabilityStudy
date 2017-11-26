library(shiny)
library(shinydashboard)
library(googlesheets)
library(dplyr)
library(DT)
library(shinyjs)

# Notification menu
notifications <- dropdownMenu(
                              
)

# Header menu
header <- dashboardHeader(title = "Visibility Tool Usability", notifications)

# Sidebar menu
sidebar <- dashboardSidebar(
#  disable = T,
  sidebarMenu(id="tabs",
    menuItemOutput("menuitem")
  )
)

# Body
body <- dashboardBody(
  #uiOutput("tabs")
  tabItems(
    tabItem("LL",uiOutput("LLview"),
            actionButton("LLcounter", "Submit")),
    tabItem("LH", uiOutput("LHtable"),
            actionButton("LHcounter", "Submit")),
    tabItem("ML", uiOutput("MLview"),
            actionButton("MLcounter", "Submit")),
    tabItem("MH", uiOutput("MHtable"),
            actionButton("MHcounter", "Submit")),
    tabItem("HL", uiOutput("HLview"),
            actionButton("HLcounter", "Submit")),
    tabItem("HH", uiOutput("HHtable"),
            actionButton("HHcounter", "Submit")),
    tabItem("lp", 
            fluidPage(
            column(width=10,
                   offset = 1,
              box(width=12, height = "100%",
                  solidHeader = FALSE,
                  useShinyjs(),
                  uiOutput("LandingPage"),
                  actionButton("sigSubmit", "Submit")
              )
            )
            )),
    tabItem("ps",uiOutput("PSview"),
            actionButton("PScounter", "Submit")),
    tabItem("end",
            radioButtons(
              "rb", "Choose one:",
              choiceNames = list(
                HTML('<img src="LL.JPG", height="350" width="1000">'),
                HTML('<img src="HM.JPG", height="800" width="1000">'),
                HTML('<img src="HL.JPG", height="400" width="1000">')
              ),
              choiceValues = list(
                "html", "html", "html"
              )
            )
            )
  )
)

dashboardPage(header, sidebar, body, skin = "purple")
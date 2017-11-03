library(shiny)
library(shinydashboard)
library(shinythemes)
library(googlesheets)
library(dplyr)

# Notification menu
notifications <- dropdownMenu(
                              
)

# Header menu
header <- dashboardHeader(title = "Visibility Tool Usability", notifications)

# Sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItemOutput("menuitem")
  )
)

# Body
body <- dashboardBody(
  #uiOutput("tabs")
  tabItems(
    tabItem("LL", h1("A was done")),
    tabItem("LH", dataTableOutput('LHtable')),
    tabItem("ML", h1("C was done")),
    tabItem("MH", dataTableOutput('MHtable')),
    tabItem("HL", h1("D was done")),
    tabItem("HH", dataTableOutput('HHtable')),
    tabItem("lp", uiOutput("MainAction"),
            actionButton("Click.Counter", "Next")),
    tabItem("ps", h1("D was done"))
  )
)

dashboardPage(header, sidebar, body, skin = "purple")

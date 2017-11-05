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
  sidebarMenu(id="tabs",
    menuItemOutput("menuitem")
  )
)

# Body
body <- dashboardBody(
  #uiOutput("tabs")
  tabItems(
    tabItem("LL", h1("A was done")),
    tabItem("LH", uiOutput("LHtable"),
            actionButton("LHcounter", "Submit")),
    tabItem("ML", h1("C was done")),
    tabItem("MH", uiOutput("MHtable"),
            actionButton("MHcounter", "Submit")),
    tabItem("HL", h1("D was done")),
    tabItem("HH", uiOutput("HHtable"),
            actionButton("HHcounter", "Submit")),
    tabItem("lp", uiOutput("MainAction"),
            actionButton("Click.Counter", "Next")),
    tabItem("ps", h1("D was done")),
    tabItem("test", h1("test hidden tab"))
  )
)

dashboardPage(header, sidebar, body, skin = "purple")

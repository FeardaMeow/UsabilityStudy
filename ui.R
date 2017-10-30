#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(googlesheets)

df.employee <- read.csv("fakeEmployee.csv")

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
    tabItem("LH", h1("B was done")),
    tabItem("ML", h1("C was done")),
    tabItem("MH", h1("D was done")),
    tabItem("HL", h1("D was done")),
    tabItem("HH", h1("D was done")),
    tabItem("lp", h1("lp was done")),
    tabItem("ps", h1("ps was done"))
  )
)

dashboardPage(header, sidebar, body, skin = "purple")

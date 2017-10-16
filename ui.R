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
library(googlesheets)

# Notification menu
notifications <- dropdownMenu(
                              
)

# Header menu
header <- dashboardHeader(title = "GE Dashboard Demo", notifications)

# Sidebar menu
sidebar <- dashboardSidebar(
  
)

# Body
body <- dashboardBody(
  
)

dashboardPage(header, sidebar, body, skin = "purple")

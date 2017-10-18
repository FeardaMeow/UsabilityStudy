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
library(googlesheets)

tabs.content <- list(list(Title = "Tab1", Content = "Tab1 content"),
                     list(Title = "Tab2", Content = "Tab2 content"),
                     list(Title = "Tab3", Content = "Tab3 content"))

menu.content <- list(menuItem("a", tabName = "a"),
                     menuItem("b", tabName = "b"),
                     menuItem("c", tabName = "c"),
                     menuItem("d", tabName = "d"))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
#  output$tabs <- renderUI({
#    
#    tabs <- lapply(1:length(tabs.content), function(i) tabPanel(tabs.content[[i]]$Title, tabs.content[[i]]$Content))
#    do.call(tabBox, tabs)
#  })
  
  output$menuitem <- renderMenu({
    sidebarMenu(
      sample(menu.content)
    )
  })
}


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

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1VSSv36D8ngNDe9TAAtU0OLBQ2JoSiTFFleqa_Y3r6GA"
ss <- googlesheets::gs_key(sheet_key)

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


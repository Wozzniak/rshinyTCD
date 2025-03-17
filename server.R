library(dplyr)
library(shinypivottabler)
library(shiny)
library(shinydashboard)
library(stringr)

server <- function(input, output, session) {
  
  
  table_indicateur <- reactive({
    df %>% 
      filter(Indicateur== str_extract(input$select, "^[0-9]+") & Croisement == str_remove(input$select, "^[0-9]+ - ")) %>% 
      select(where(~ any(!is.na(.)))) %>% 
      select(-Indicateur_Croisement)
  }) 
  
  callModule(module = shinypivottabler,
             id = "id",
             show_title = FALSE,
             theme = theme,
             data = table_indicateur)
}
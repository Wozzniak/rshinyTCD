library(dplyr)
library(shinypivottabler)
library(shiny)
library(shinydashboard)
library(stringr)

server <- function(input, output, session) {
  
  
  table_indicateur <- reactive({
    df %>% 
      dplyr::filter(Indicateur== stringr::str_extract(input$select, "^[0-9]+") & Croisement == stringr::str_remove(input$select, "^[0-9]+ - ")) %>% 
      dplyr::select(where(~ any(!is.na(.)))) %>% 
      dplyr::select(-Indicateur_Croisement)
  }) 
  
  callModule(module = shinypivottabler,
             id = "id",
             show_title = FALSE,
             theme = theme,
             data = table_indicateur)
}
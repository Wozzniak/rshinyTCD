library(dplyr)
library(shinypivottabler)
library(shiny)
library(shinydashboard)
library(stringr)

# create artificial dataset

# theme <- list(
#   fontName="arial",
#   fontSize="1em",
#   headerBackgroundColor = "#FF7F50",
#   headerColor = "#FF7F50",
#   cellBackgroundColor = "#FF7F50",
#   cellColor = "#FF7F50",
#   outlineCellBackgroundColor = "#FF7F50",
#   outlineCellColor = "#FF7F50",
#   totalBackgroundColor = "#FF7F50",
#   totalColor = "#FF7F50",
#   borderColor = "#000000"
# )


data <- readRDS("./BDS.rds")

df <- as.data.frame(matrix(NA, nrow = 8, ncol = 211))
colnames(df) <- colnames(data)
# random_row_1 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_2 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_3 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_4 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_5 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_6 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_7 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# random_row_8 <- as.data.frame(matrix(runif(211, min = 0, max = 100), nrow = 1))
# # Ensure column names match
# colnames(random_row_1) <- colnames(data)
# colnames(random_row_2) <- colnames(data)
# colnames(random_row_3) <- colnames(data)
# colnames(random_row_4) <- colnames(data)
# colnames(random_row_5) <- colnames(data)
# colnames(random_row_6) <- colnames(data)
# colnames(random_row_7) <- colnames(data)
# colnames(random_row_8) <- colnames(data)

# Append the row
#df <- rbind(data, random_row_1, random_row_2, random_row_3, random_row_4, random_row_5, random_row_6, random_row_7, random_row_8)
df[1,1] <- "2023"
df[1,2] <- "CSAM MEF"
df[1,3] <- "6210"
df[1,4] <- "SEXE ET CATEGORIE"
df[1,5] <- "80"
df[1,6] <- "A"
df[1,8] <- "F"
df[1,9] <- "Féminin"
df[2,1] <- "2023"
df[2,2] <- "CSAM MEF"
df[2,3] <- "6210"
df[2,4] <- "SEXE ET CATEGORIE"
df[2,5] <- "60"
df[2,6] <- "A"
df[2,8] <- "M"
df[2,9] <- "Masculin"
df[3,1] <- "2023"
df[3,2] <- "CSAM MEF"
df[3,3] <- "6210"
df[3,4] <- "SEXE ET CATEGORIE"
df[3,5] <- "42"
df[3,6] <- "B"
df[3,8] <- "F"
df[3,9] <- "Féminin"
df[4,1] <- "2023"
df[4,2] <- "CSAM MEF"
df[4,3] <- "6210"
df[4,4] <- "SEXE ET CATEGORIE"
df[4,5] <- "19"
df[4,6] <- "B"
df[4,8] <- "M"
df[4,9] <- "Masculin"
df[5,1] <- "2023"
df[5,2] <- "CSAM MEF"
df[5,3] <- "6211"
df[5,4] <- "CORPS ET AGE"
df[5,5] <- "62"
df[5,10] <- "322"
df[5,15] <- "[25 - 35["
df[6,1] <- "2023"
df[6,2] <- "CSAM MEF"
df[6,3] <- "6211"
df[6,4] <- "CORPS ET AGE"
df[6,5] <- "17"
df[6,10] <- "322"
df[6,15] <- "[35 - 45["
df[7,1] <- "2023"
df[7,2] <- "CSAM MEF"
df[7,3] <- "6211"
df[7,4] <- "CORPS ET AGE"
df[7,5] <- "42"
df[7,10] <- "322"
df[7,15] <- "[45 - 55["
df[8,1] <- "2023"
df[8,2] <- "CSAM MEF"
df[8,3] <- "6211"
df[8,4] <- "CORPS ET AGE"
df[8,5] <- "19"
df[8,10] <- "322"
df[8,15] <- "[55 - 65["

df[,5] <- as.numeric(df[,5])

df <- df %>% 
  mutate(Indicateur_Croisement=paste0(Indicateur," - ",Croisement))

indicateur_croisement <- unique(df$Indicateur_Croisement)


theme <- list(
  fontName="arial",
  fontSize="1em",
  headerBackgroundColor = "#558299",
  headerColor = "#FFFFFF",
  cellBackgroundColor = "#FFFFFF",
  cellColor = "#000000",
  outlineCellBackgroundColor = "#558299",
  outlineCellColor = "#558299",
  totalBackgroundColor = "#F37655",
  totalColor = "#000000",
  borderColor = "#000000"
)

ui <- dashboardPage(
  
  dashboardHeader(
    title="Indicateurs de la BDS"
  ),
  
  dashboardSidebar(
    selectInput("select", "Indicateur et croisement :",
                choices=indicateur_croisement),
    
    tags$head(
      tags$style(HTML("
      body {
        background-color: #F7F2F9 ; 
      }
    "))
    )),
  dashboardBody(
    shinypivottablerUI(id = "id",
                       app_colors = c("#F0811F", "#152969"),
                       app_linewidth = 3)
  ))


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

shinyApp(ui, server)

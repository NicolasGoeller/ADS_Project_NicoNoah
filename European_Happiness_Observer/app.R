library(shiny)
library(shinydashboard)
library(tidyverse)

#Reading in data and subsetting it for better handling
EVS_final <- read_rds("Data/EVS_final.rds")
shiny_data <- select(EVS_final, sat, edu_cat)


ui <- dashboardPage(
  dashboardHeader(title = "European Happiness Observer", titleWidth = "300"),
  dashboardSidebar(width = "300",
    sidebarMenu(
      menuItem("Mapped dashboards", tabName = "map", icon = icon("globe europe", lib = "font-awesome")),
        #menuSubItem("National dashboard"),
        #menuSubItem("Regional dashboard"),
      menuItem("Plot for detailed analysis", tabName = "plots", icon = icon("stats", lib = "glyphicon")),
      menuItem("Raw data", tabName = "data", icon = icon("floppy-disk", lib = "glyphicon")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map", h1("Geographical overview")),
      
      tabItem(tabName = "plots", h1("Plots for detailed analysis"),
              fluidRow(
                box(title = "Bar chart", status = "primary", solidHeader = T, 
                    plotOutput("barchart")),
                box(title = "Controls for bar chart", status = "warning", solidHeader = T 
                    ))),
      tabItem(tabName = "data", h1("Raw data table"))
    )#Closing tabItems
   )#Closing dashboardBody
  )
 

#varSelectInput()
server <- function(input, output) {
  output$barchart <- renderPlot({
    ggplot(shiny_data, aes(x = sat))+
      geom_bar()+
      #from next line, publication ready & nice
      labs(x = "Life satisfaction")+
      theme_ipsum(grid = "Y")
  })
}

shinyApp(ui, server)


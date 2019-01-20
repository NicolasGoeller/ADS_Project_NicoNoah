library(shiny)
library(shinydashboard)
library(tidyverse)
library(hrbrthemes)

#Reading in data and subsetting it for better handling
EVS_final <- read_rds("Data/EVS_final.rds")
shiny_data <- select(EVS_final, sat, edu_cat, nation)

nat <- c("None", "Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")
nat <- as.list(nat)

ui <- dashboardPage(
  dashboardHeader(title = "European Happiness Observer", titleWidth = "300"),
  dashboardSidebar(width = "300",
    sidebarMenu(
      menuItem("Mapped dashboards", tabName = "map", icon = icon("globe europe", lib = "font-awesome")),
        #menuSubItem("National dashboard"),
        #menuSubItem("Regional dashboard"),
      menuItem("Graphics for detailed analysis", tabName = "plots", icon = icon("stats", lib = "glyphicon")),
        
      menuItem("Raw data", tabName = "data", icon = icon("floppy-disk", lib = "glyphicon")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map", h1("Geographical overview")),
      
      tabItem(tabName = "plots", h1("Plots for detailed analysis"),
              fluidRow(
                box(title = "Bar chart", status = "primary", solidHeader = T, width = 8,
                    plotOutput("barchart")),
                box(title = "Controls for bar chart", status = "warning", solidHeader = T, 
                    width = 4, varSelectInput("variable", "Variable:", shiny_data), 
                    selectInput("country", "Country:", nat))
                #box(title = "Line plot")
                )),
      tabItem(tabName = "data", h1("Raw data table"))
    )#Closing tabItems
   )#Closing dashboardBody
  )



server <- function(input, output) {
  output$barchart <- renderPlot({
    if(!!input$country == "None"){
      ggplot(shiny_data, aes(!!input$variable))+
        geom_bar()+
        labs(x = "Variable")+
        theme_ipsum(grid = "Y") +
        coord_flip()
    }else {
      shiny_data %>% 
      filter(nation == input$country) %>% 
    ggplot(aes(!!input$variable))+
      geom_bar()+
      labs(x = "Variable")+
      theme_ipsum(grid = "Y") +
      coord_flip()
    }
  })
}

shinyApp(ui, server)

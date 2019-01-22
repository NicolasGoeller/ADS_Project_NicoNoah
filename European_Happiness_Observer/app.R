install.packages(c("shiny", "shinydashboard"))
library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(stargazer)

#Reading in data and subsetting it for better handling
EVS_final <- read_rds("Data/EVS_final.rds")
shiny_data <- select(EVS_final, sat, edu_cat, nation, eureg, siops)
shiny_data %<>% within({
  Life_satisfaction <- sat
  Education_categories <- edu_cat
  Country_of_residence <- nation
  Geographical_region <- eureg
  SIOPS_Index <- siops
})
shiny_data <- select(shiny_data, Life_satisfaction, Education_categories, Country_of_residence,
                     Geographical_region, SIOPS_Index)
shiny_data %<>% na.omit
nat <- c("None", "Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")
at <- as.list(nat)

eureg <- c("None", "Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe")
eureg <- as.list(eureg)

ui <- dashboardPage(
  dashboardHeader(title = "European Happiness Observer", titleWidth = "300"),
  dashboardSidebar(width = "300",
    sidebarMenu(
      menuItem("Mapped dashboards", tabName = "map", icon = icon("globe europe", lib = "font-awesome")),
        #menuSubItem("National dashboard"),
        #menuSubItem("Regional dashboard"),
      menuItem("Graphics for detailed analysis", tabName = "graphics", icon = icon("stats", lib = "glyphicon")),
        menuSubItem("Univariate bar charts", tabName = "bar"),
        menuSubItem("Bivariate line plots", tabName = "plots"),
        menuSubItem("Regression tables", tabName = "regress"),
      menuItem("Raw data", tabName = "data", icon = icon("floppy-disk", lib = "glyphicon")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map", h1("Geographical overview")),
      
      tabItem(tabName = "graphics", h1("Graphics for detailed analysis")),
      
      tabItem(tabName = "bar", h1("Univariate bar chart to customize"),
              fluidRow(
                box(title = "Bar chart", status = "primary", solidHeader = T, width = 8,
                    plotOutput("barchart")),
                box(title = "Controls for bar chart", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variable for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput("variable", label = "Variable:", shiny_data), 
                    selectInput("country", "Country:", nat),
                    selectInput("eureg", "Geographical region:", eureg))
                )),
      
      tabItem(tabName = "plots", h1("Bivariate line plot to customize"),
              fluidRow(
                box(title = "Line plot", status = "primary", solidHeader = T, width = 8,
                    plotOutput("lineplot")),
                box(title = "Controls for line plot", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variables for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput("xvar", "Variable on x-axis:", shiny_data),
                    varSelectInput("yvar", "Variable on y-axis:", shiny_data),
                    selectInput("country2", "Country:", nat),
                    selectInput("eureg2", "Geographical region:", eureg))
              )),
      
      tabItem(tabName = "regress", h1("Linear, mixed-effects models to customize"),
              fluidRow(
                box(title = "Regression table", status = "primary", solidHeader = T, width = 8,
                    verbatimTextOutput("regtab")),
                box(title = "Controls for regression", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variables for modeling, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput("DV", "Dependent variable:", shiny_data),
                    varSelectInput("IDV", "Independent variable:", shiny_data),
                    selectInput("country3", "Country:", nat),
                    selectInput("eureg3", "Geographical region:", eureg))
              )),
      
      tabItem(tabName = "data", h1("Raw data table"))
      
    )#Closing tabItems
   )#Closing dashboardBody
  )

server <- function(input, output) {
  
  output$barchart <- renderPlot({
    if(!!input$country == "None" & !!input$eureg == "None"){
      ggplot(shiny_data, aes(!!input$variable))+
        geom_bar(color = "grey58", fill = "grey58")+
        labs(x = paste(input$variable))+
        theme_ipsum(grid = "Y") +
        coord_flip()
    }else if(!!input$country == "None" & !!input$eureg != "None"){
      shiny_data %>% 
        filter(Geographical_region == input$eureg) %>% 
        ggplot(aes(!!input$variable))+
        geom_bar(color = "grey58", fill = "grey58")+
        labs(x = paste(input$variable))+
        theme_ipsum(grid = "Y") +
        coord_flip()
    }else if(!!input$country != "None" & !!input$eureg == "None"){
      shiny_data %>% 
      filter(Country_of_residence == input$country) %>% 
    ggplot(aes(!!input$variable))+
      geom_bar(color = "grey58", fill = "grey58")+
      labs(x = paste(input$variable))+
      theme_ipsum(grid = "Y") +
      coord_flip()
    }
  })
  
  output$lineplot <- renderPlot({
    if(!!input$country2 == "None" & !!input$eureg2 == "None"){
      ggplot(shiny_data, aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar))+
        theme_ipsum(grid = "Y")
    }else if(!!input$country2 == "None" & !!input$eureg2 != "None"){
      shiny_data %>% 
        filter(Geographical_region == input$eureg2) %>% 
        ggplot(aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar))+
        theme_ipsum(grid = "Y")
    }else if(!!input$country2 != "None" & !!input$eureg2 == "None"){
      shiny_data %>% 
        filter(Country_of_residence == input$country2) %>% 
        ggplot(aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar))+
        theme_ipsum(grid = "Y")
    }
  })
  
  output$regtab <- renderTable({
   lm(input$DV ~ input$IDV) %>% 
      summary()
    
  })
}

shinyApp(ui, server)


#renderPrint({
#  if(!!input$country3 == "None" & !!input$eureg3 == "None"){
#    lm(!!input$DV ~ !!!input$IDV) %>% 
#      stargazer(#regression models 
#        type = "text", # character vector (eg. "text" / "html" / "latex")
#        title = "Linear regression model",  # header
#        style = "default",  # style (choice see below)
#        summary = NULL,  # logical vector: output summary statistics when given data.frame
#        out = "table1.html", # path and output of file
#        out.header = FALSE, # logical vector: should output file contain code-header?
#        column.labels = c("Linear model"), # column labels for mod1/mod2
#        column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
#        dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
#        star.cutoffs = c(0.05,0.01,0.001),
#        dep.var.labels = c(paste(input$DV)))
#  }else if(!!input$country3 == "None" & !!input$eureg3 == "None"){
#    shiny_data %>% 
#      filter(Geographical_region == input$eureg3) %>%
#      
#      stargazer
#  }else if(!!input$country3 == "None" & !!input$eureg3 == "None"){
#    shiny_data %>% 
#      filter(Country_of_residence == input$country3) %>% 
#      stargazer
#  }
#})


stargazer(#regression models 
  type = "text", # character vector (eg. "text" / "html" / "latex")
  title = "Linear regression model",  # header
  style = "default",  # style (choice see below)
  summary = NULL,  # logical vector: output summary statistics when given data.frame
  out = "table1.html", # path and output of file
  out.header = FALSE, # logical vector: should output file contain code-header?
  column.labels = c("Linear model"), # column labels for mod1/mod2
  column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
  dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
  star.cutoffs = c(0.05,0.01,0.001),
  dep.var.labels = c(paste(input$DV)))




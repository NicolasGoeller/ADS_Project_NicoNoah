#install.packages(c("shiny", "shinydashboard", "tidyverse", "magrittr", "hrbrthemes", 
#                    "stargazer", "lme4", "viridis"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(lme4)
library(stargazer)

#------------------------------------------------------------------------------------

#Reading in data and subsetting it for better handling
EVS_final <- read_rds("Data/EVS_final.rds")

shiny_data <- dplyr::select(EVS_final, 
                            sat, edu_cat, siops, job_sat, inc_mon, nowork, inst_trust,
                            trust_wrth, sex, age, mar_stat, town, intp_trust,
                            nation, eureg, reg, 
                            hdi, gini, fhrate, unemployment, GDPpc,
                            trust_wrth_reg, intp_trust_reg, inst_trust_reg)
shiny_data %<>% within({
  Life_satisfaction <- sat
  Education_categories <- edu_cat
  SIOPS_Index <- siops
  Job_satisfaction <- job_sat
  Monthly_income <- inc_mon
  Unemployed <- nowork
  Institutional_trust <- inst_trust
  Trustworthiness <- trust_wrth
  Trust_interpersonal <- intp_trust
  Sex <- sex
  Age <- age
  Marital_status <- mar_stat
  Town_size <- town
  
  Country_of_residence <- nation
  Region_of_residence <- reg
  Geographical_region <- eureg
  
  Human_Development_Index <- hdi
  Unemployment_rate <- unemployment
  Gini_coefficient <- gini
  GDP_per_capita <- GDPpc
  Freedom_House_Democracy <- fhrate
  
  Regional_institutional_trust <- inst_trust_reg
  Regional_norm_salience <- trust_wrth_reg
  Regional_interpersonal_trust <- intp_trust_reg
})
shiny_data <- dplyr::select(shiny_data, 
                            Life_satisfaction, Education_categories, SIOPS_Index, Job_satisfaction,
                            Unemployed, Institutional_trust, Trustworthiness, 
                            Trust_interpersonal, Sex, Age, Marital_status, Town_size, #12
                            Country_of_residence, Region_of_residence, Geographical_region, #15
                            Human_Development_Index, Unemployment_rate, Gini_coefficient, 
                            GDP_per_capita, Freedom_House_Democracy, #20
                            Regional_institutional_trust, Regional_norm_salience, Regional_interpersonal_trust)#23
#shiny_data %<>% na.omit

nat_geodata <- read_rds("Data/Nation_geoData.rds")
shiny_nat <- dplyr::select(nat_geodata, life_sat, unemployment, GDPpc, gini, hdi, fhrate, nation, geometry, X, Y)
shiny_nat %<>% within({
  Aggregated_life_satisfaction <- life_sat
  Human_Development_Index <- hdi
  Unemployment_rate <- unemployment
  Gini_coefficient <- gini
  GDP_per_capita <- GDPpc
  Freedom_House_Democracy <- fhrate
  Country_of_residence <- nation
})
shiny_nat <- dplyr::select(shiny_nat, Aggregated_life_satisfaction, Human_Development_Index,
                           Unemployment_rate, Gini_coefficient, GDP_per_capita,
                           Freedom_House_Democracy, Country_of_residence, geometry, X, Y)

#----------------------------------------------------------------------------

#Creating list objects necessary for nice variable selection

nat <- c("None", "Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")
nat <- as.list(nat)

eureg <- c("None", "Northern Europe", "Western Europe", "Southern Europe", "Eastern Europe")
eureg <- as.list(eureg)

#-----------------------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "European Happiness Observer", titleWidth = "300"),
  dashboardSidebar(width = "300",
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-sign", lib = "glyphicon")),
      menuItem("Mapped dashboards", tabName = "map", icon = icon("globe europe", lib = "font-awesome")),
        menuSubItem("Regional dashboard", tabName = "regmap"),
      menuItem("Graphics for detailed analysis", tabName = "graphics", icon = icon("stats", lib = "glyphicon")),
        menuSubItem("Univariate bar charts", tabName = "bar"),
        menuSubItem("Bivariate line plots", tabName = "plots"),
        menuSubItem("Boxplots", tabName = "box"),
        menuSubItem("Regression tables", tabName = "regress"),
      menuItem("Raw data", tabName = "data", icon = icon("floppy-disk", lib = "glyphicon")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h1("Introduction to the European Happiness Observer")),
      tabItem(tabName = "map", h1("Geographical overview"),
              fluidRow(
                box(title = "Europe map plot for nations", status = "primary", solidHeader = T, width = 9,
                    plotOutput("map")),
                box(title = "Controls for map plot", status = "warning", solidHeader = T, 
                    width = 3,
                    "Choose your variable for plotting",br(), br(),
                    varSelectInput("macro", "Variable:", shiny_nat[,1:7]))
                #box(title = "Data indicator", status = "success", solidHeader = T, width = 3,
                #    selectInput("nat", choices = nat))
              )),
      tabItem(tabName = "regmap", h1("Geographical overview"),
              fluidRow(
                box(title = "Europe map plot for regions", status = "primary", solidHeader = T, width = 9,
                    plotOutput("regiomap")),
                box(title = "Controls for map plot", status = "warning", solidHeader = T, 
                    width = 3,
                    "Choose your variable for plotting",br(), br(),
                    varSelectInput("regio", "Variable:", shiny_nat[,1:7]))
              )),
      
      tabItem(tabName = "graphics", h1("Graphics for detailed analysis")),
      
      tabItem(tabName = "bar", h1("Univariate bar chart to customize"),
              fluidRow(
                box(title = "Bar chart", status = "primary", solidHeader = T, width = 8,
                    plotOutput("barchart")),
                box(title = "Controls for bar chart", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variable for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput(inputId = "variable", label = "Variable:", shiny_data[,1:12]), 
                    selectInput(inputId = "country", label = "Country:", nat),
                    selectInput(inputId = "eureg", label = "Geographical region:", eureg))
                )),
      
      tabItem(tabName = "plots", h1("Bivariate line plot to customize"),
              fluidRow(
                box(title = "Line plot", status = "primary", solidHeader = T, width = 8,
                    plotOutput("lineplot")),
                box(title = "Controls for line plot", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variables for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput("xvar", "Variable on x-axis:", shiny_data[,1:12]),
                    varSelectInput("yvar", "Variable on y-axis:", shiny_data[,1:12]),
                    selectInput("country2", "Country:", nat),
                    selectInput("eureg2", "Geographical region:", eureg))
              )),
      
      tabItem(tabName = "box", h1("Boxplots to customize"),
              fluidRow(
                box(title = "Boxplot", status = "primary", solidHeader = T, width = 8,
                    plotOutput("boxplot")),
                box(title = "Controls for boxplot", status = "warning", solidHeader = T, 
                    width = 4,
                    varSelectInput("group", "Plot grouped by:", shiny_data[,13:15]),
                    varSelectInput("observ", "Variable to observe:", shiny_data[,1:12]),
                    varSelectInput("order", "Variable to order groups:", shiny_data[,16:23]))
              )),
      
      tabItem(tabName = "regress", h1("Linear, mixed-effects models to customize"),
              fluidRow(
                box(title = "Regression table", status = "primary", solidHeader = T, width = 8,
                    uiOutput("regtab")),
                box(title = "Controls for regression", status = "warning", solidHeader = T, 
                    width = 4,
                    varSelectInput("DV", "Dependent variable:", shiny_data[,1:12]),
                    varSelectInput("IDV", "Independent variable:", shiny_data[,1:12]),
                    checkboxGroupInput("mixreg", "Multilevel features:", selected = "None", 
                                       choices = c("None", "National level", "Regional level", "Both levels")),
                    varSelectInput("natvar", "Variables on national level:", shiny_data[,16:20]),
                    varSelectInput("regvar", "Variables on regional level:", shiny_data[,21:23]))
              )),
      
      tabItem(tabName = "data", h1("Raw data table"), 
              fluidRow(
                box(title = "Raw data table", status = "primary", solidHeader = T, width = 12,
                  dataTableOutput("data"))
                #box(title = "Data selection", status = "warning", solidHeader = T, width = 2,
                #checkboxGroupInput("datacheck", label = "Choose the data to be shown:", 
                #                   choices = c("Individual data", "Regional data", "National data"),
                #                   selected = "Individual data"))
              ))
                
      )#Closing tabItems
    )#Closing dashboardBody
  )

server <- function(input, output) {
  
  output$map <- renderPlot({
    if(!!input$macro != "Country_of_residence"){
      ggplot(data = shiny_nat)+
      geom_sf(aes(fill = !!input$macro))+
      labs(fill = paste(input$macro))+
      scale_fill_viridis_c()+
      coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
    } else {
      ggplot(data = shiny_nat)+
        geom_sf(aes(fill = !!input$macro))+
        labs(fill = paste(input$macro))+
        scale_fill_viridis_d()+
        coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
    }
  })
  
  output$regiomap <- renderPlot({})
  
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
    }else{
      text = paste("\n   You chose from both 'Country:' and 'Geographical region:'.\n",
                   "       Please do only select from one of those at a time.\n",
                   "       To deselect, put the dropdown on 'None'.")
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = text) + 
        theme_void() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
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
    }else{
      text = paste("\n   You chose from both 'Country:' and 'Geographical region:'.\n",
                   "       Please do only select from one of those at a time.\n",
                   "       To deselect, put the dropdown on 'None'.")
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = text) + 
        theme_void() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
    }
  })
  
  output$boxplot <- renderPlot({
    ggplot(shiny_data, aes(x = reorder(!!input$group, !!input$order), y = !!input$observ))+
      geom_boxplot()+
      coord_flip() +
      labs(x = paste(input$group, "after", input$order), y = paste(input$observ))
      #theme_ipsum(grid = "Y")
  })
  
  output$regtab <- renderUI({
    
    if(!!input$mixreg == c("National level")){
      reg <- paste(paste(input$DV), "~", paste(input$IDV), "+", paste(input$natvar), 
                   "+(1|Country_of_residence)", sep = "")
      lmer(as.formula(reg), shiny_data) %>%
        stargazer( #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Multilevel linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "European_Happiness_Observer/table1.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Multilevel model for nations"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c(gsub("_", " ",paste(input$DV)))) 
      tab <- htmlTemplate(filename = "European_Happiness_Observer/table1.html") 
      
    }else if(!!input$mixreg == c("Regional level")){
      reg <- paste(paste(input$DV), "~", paste(input$IDV), "+", paste(input$regvar), 
                   "+(1|Region_of_residence)", sep = "")
      lmer(as.formula(reg), shiny_data) %>%
        stargazer( #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Multilevel linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "European_Happiness_Observer/table1.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Multilevel model for regions"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c(gsub("_", " ",paste(input$DV)))) 
      tab <- htmlTemplate(filename = "European_Happiness_Observer/table1.html")
      
    }else if(!!input$mixreg == c("Both levels")){
      reg <- paste(paste(input$DV), "~", paste(input$IDV), "+", paste(input$natvar), "+", paste(input$regvar), 
                   "+", "+(1|Country_of_residence)", "+(1|Region_of_residence)", sep = "")
      lmer(as.formula(reg), shiny_data) %>%
        stargazer( #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Multilevel linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "European_Happiness_Observer/table1.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Multilevel model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c(gsub("_", " ",paste(input$DV)))) 
      tab <- htmlTemplate(filename = "European_Happiness_Observer/table1.html")
      
    }else if(!!input$mixreg == c("None")){
      reg <- paste(paste(input$DV), paste(input$IDV), sep = "~")
      lm(as.formula(reg), shiny_data) %>%
        stargazer(  #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "European_Happiness_Observer/table1.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Basic model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          dep.var.caption = "Dependent variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c(gsub("_", " ",paste(input$DV)))) 
      tab <- htmlTemplate(filename = "European_Happiness_Observer/table1.html")
    }
    
  })
  
  output$data <- renderDataTable(shiny_data, escape = T, searchDelay = 20)
  
}

shinyApp(ui, server)

#End of app code##------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

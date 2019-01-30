#install.packages(c("shiny", "shinydashboard", "tidyverse", "magrittr", "hrbrthemes", 
#                    "stargazer", "lme4", "viridis"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(lme4)
library(stargazer)
library(viridis)

#------------------------------------------------------------------------------------

#Reading in data and subsetting it for better handling
EVS_final <- read_rds("Data/EVS_final.rds")

shiny_data <- dplyr::select(EVS_final, 
                            sat, happ, siops, job_sat, fair,
                            intp_trust,inst_trust, trust_wrth,
                            work_impo, free_job, talent, duty, work_first, sup, 
                            entre, nowork,
                            isei, inc_cat, inc_an, inc_mon, incppp_mon, inc_eq,
                            sex, age, age_cat, mar_stat, edu_cat3, edu_cat7, edu, town, 
                            nation, eureg, reg, 
                            hdi, gini, fhrate, unemployment, GDPpc, life_sat,
                            trust_wrth_reg, intp_trust_reg, inst_trust_reg, fair_reg)
shiny_data %<>% within({
  Life_satisfaction <- sat
  Happiness <- happ
  SIOPS_Index <- siops
  Job_satisfaction <- job_sat
  Fairness_of_other_people <- fair
  
  Institutional_trust <- inst_trust
  Trustworthiness <- trust_wrth
  Trust_interpersonal <- intp_trust
  
  Importance_of_work <- work_impo
  Freedom_in_job_decisions <- free_job
  Job_talent_development <- talent
  Work_as_societal_duty <- duty
  Work_devotion <- work_first
  Supervisory_work <- sup
  Entrepreneurial_work <- entre
  Unemployed <- nowork
  
  ISEI_Index <- isei
  Annual_income <- inc_an
  Income_categories <- inc_cat
  Monthly_income <- inc_mon
  Monthly_income_ppp <- incppp_mon
  Equality_of_income_incentives <- inc_eq
  
  Sex <- sex
  Age <- age
  Age_categories <- age_cat
  Education_categories <- edu_cat3
  Education <- edu_cat7
  Education_scale <- edu
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
  Aggregated_life_satisfaction <- life_sat
  
  Regional_institutional_trust <- inst_trust_reg
  Regional_norm_salience <- trust_wrth_reg
  Regional_interpersonal_trust <- intp_trust_reg
  Regional_expected_fairness <- fair_reg
})
shiny_data <- dplyr::select(shiny_data, #from now: 1:30
                            Life_satisfaction, Happiness, SIOPS_Index, Job_satisfaction, #4
                            Fairness_of_other_people, Institutional_trust, Trustworthiness, #7 
                            Trust_interpersonal, Importance_of_work, Freedom_in_job_decisions, #10
                            Job_talent_development, Work_as_societal_duty, Work_devotion, #13
                            Supervisory_work, Entrepreneurial_work, Unemployed, #16
                            ISEI_Index, Annual_income, Income_categories, Monthly_income, #20 
                            Monthly_income_ppp, Equality_of_income_incentives, #22
                            Sex, Age, Age_categories, Education_categories, Education, #27 
                            Education_scale, Marital_status, #29
                            Town_size, #from now: 31:33
                            Country_of_residence, Region_of_residence, 
                            Geographical_region, #from now: 34:39
                            Human_Development_Index, Unemployment_rate, Gini_coefficient,
                            GDP_per_capita, Freedom_House_Democracy, 
                            Aggregated_life_satisfaction, #from now: 40:43
                            Regional_institutional_trust, Regional_norm_salience,
                            Regional_interpersonal_trust, Regional_expected_fairness)
#shiny_data %<>% na.omit

shiny_nat <- read_rds("Data/Nation_geoData.rds")
shiny_nat <- dplyr::select(shiny_nat, life_sat, unemployment, GDPpc, gini, hdi, fhrate, nation, geometry, X, Y)
shiny_nat %<>% within({
  Aggregated_life_satisfaction <- life_sat
  Human_Development_Index <- hdi
  Unemployment_rate <- unemployment
  Gini_coefficient <- gini
  GDP_per_capita <- GDPpc
  Freedom_House_Democracy <- fhrate
  Country_of_residence <- nation
  #Geographical_region <- eureg
})
shiny_nat <- dplyr::select(shiny_nat, Aggregated_life_satisfaction, Human_Development_Index,
                           Unemployment_rate, Gini_coefficient, GDP_per_capita,
                           Freedom_House_Democracy, Country_of_residence, #Geographical_region, 
                           geometry, X, Y)

shiny_reg <- read_rds("Data/Region_geoData.rds")
shiny_reg <- dplyr::select(shiny_reg, trust_wrth_reg, intp_trust_reg, 
                           inst_trust_reg, fair_reg, reg, nation, #eureg,
                           geometry) #, X, Y

shiny_reg %<>% within({
  Regional_institutional_trust <- inst_trust_reg
  Regional_norm_salience <- trust_wrth_reg
  Regional_interpersonal_trust <- intp_trust_reg
  Regional_expected_fairness <- fair_reg
  Region_of_residence <- reg
  Country_of_residence <- nation
  #Geographical_region <- eureg
})

shiny_reg <- dplyr::select(shiny_reg, 
                           Regional_institutional_trust, Regional_norm_salience,
                           Regional_interpersonal_trust, Regional_expected_fairness,
                           Region_of_residence, Country_of_residence, #Geographical_region,
                           geometry) #, X, Y

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

reg_new <- c("None", "Ostösterreich", "Südösterreich", "Westösterreich",
             "Région de Bruxelles-Capitale / Brussels Hoofdstedelijk Gewest",
             "Vlaams Gewest", "Région Wallonne",
             "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
             "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
             "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
             "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen",
             "Noroeste", "Noreste", "Comunidad de Madrid", "Centro (E)", "Este",
             "Sur", "Canarias",
             "Île de France", "Bassin Parisien", "Nord - Pas-de-Calais", "Est",
             "Ouest", "Sud-Ouest", "Centre-Est", "Méditerranée",
             "Közép-Magyarország", "Dunántúl", "Alföld És Észak",
             "Nord-Ovest", "Nord-Est", "Centro (I)", "Sud", "Isole",
             "Noord-Nederland", "Oost-Nederland", "West-Nederland", "Zuid-Nederland",
             "Region Centralny", "Region Poludniowy", "Region Wschodni", "Region Pólnocno-Zachodni",
             "Region Poludniowo-Zachodni", "Region Pólnocny",
             "Macroregiunea unu", "Macroregiunea doi", "Macroregiunea trei",
             "Macroregiunea patru",
             "Östra Sverige", "Södra Sverige", "Norra Sverige",
             "North East (England)", "North West (England)", "Yorkshire and the Humber",
             "East Midlands (England)", "West Midlands (England)", "East of England",
             "London", "South East (England)", "South West (England)", "Wales",
             "Scotland", "Northern Ireland",
             "Voreia Ellada", "Kentriki Ellada", "Attiki", "Nisia Aigaiou, Kriti",
             "West (UA)","Centre (UA)", "North (UA)", "East (UA)", "South (UA)",
             "Kypros / Kibris", "Northern Cyprus",
             "Centralna Srbija", "Vojvodina",
             "Severna i iztochna Bulgaria", "Yugozapadna i yuzhna tsentralna Bulgaria",
             "Schweiz/Suisse/Svizzera", "Ceská Republika", "Danmark", "Eesti",
             "Manner-Suomi", "Hrvatska", "Ireland", "Ísland", "Lietuva", "Luxembourg (Grand-Duché)",
             "Latvija", "Malta", "Norge", "Continente", "Slovenija", "Slovenská Republika",
             "Belarus", "Turkey", "Georgia", "Moldova", "Montenegro", "Armenia",
             "Bosna i Hercegovina", "Albania", "Poranesnata jugoslovenska Republika Makedonija",
             "Kosovo")
reg <- as.list(reg_new)

#-----------------------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "European Happiness Observer", titleWidth = "300"),
  dashboardSidebar(width = "300",
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-sign", lib = "glyphicon")),
      menuItem("Mapped dashboards", tabName = "map", icon = icon("globe europe", lib = "font-awesome")),
        menuSubItem("Regional dashboard", tabName = "regmap"),
      menuItem("Graphics for detailed analysis", tabName = "graphics", icon = icon("stats", lib = "glyphicon")),
        menuSubItem("Bar charts", tabName = "bar"),
        menuSubItem("Line graphs", tabName = "plots"),
        menuSubItem("Boxplots", tabName = "box"),
        menuSubItem("Regression tables", tabName = "regress"),
      menuItem("Raw data", tabName = "data", icon = icon("floppy-disk", lib = "glyphicon")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h1("Introduction to the European Happiness Observer"),
              fluidRow(
                box(title = "Foreword", status = "primary", width = 12,
                    "The ‘European Happiness Observer‘ is a project with the aim
                    to provide an easy access to analysis and visualisation of 
                    data on individual and structural determinants of life satisfaction
                    for people in European countries. Data was acquired from different 
                    sources to reflect pecuniary and non-pecuniary elements on both the
                    individual level and the structural level of region and country
                    of the individual’s residence. Interested users can employ
                    geoplots on national and regional level for the respective 
                    structural variables. Additionally, a range of visualisations
                    consisting of bar charts, line graphs and boxplots are free to 
                    customize through the user. Furthermore, raw data is accessible
                    in table format for examination. Lastly, regression models of 
                    regular linear and mixed-effects nature can be composed for more
                    extensive analysis. All interfaces are designed to be workable 
                    with minimal experience in data handling, however interpretation
                    of results may require some knowledge of the topic, depending on
                    the analysis tool in question.")
              )),
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
      
      tabItem(tabName = "graphics", h1("Graphics for detailed analysis"),
              fluidRow(
                box(title = "Explanation", status = "primary", solidHeader = T, width = 4,
                    "This section is dedicated for analysis of variables. The bar charts and
                    line graphs are designed to be accessible for beginners, too. More 
                    experienced users may also use the boxplots and regression tables to get
                    deeper insights into the data. Regarding questions of coding and sourcing 
                    of data, users are asked to use the codebook. Details for interpretation, 
                    especially regarding the multilevel model in 'Regression tables'."),
                box(title = "Codebook", status = "warning", solidHeader = T, width = 8,
                    downloadButton("down", "Download the codebook as txt:"), br(),
                    textOutput("cdb"))
              )),
      
      tabItem(tabName = "bar", h1("Bar chart to customize"),
              fluidRow(
                box(title = "Bar chart", status = "primary", solidHeader = T, width = 8,
                    plotOutput("barchart")),
                box(title = "Controls for bar chart", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variable for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput(inputId = "variable", label = "Variable:", shiny_data[,1:30]),
                    checkboxInput("check", "Allow for in-chart grouping"),
                    varSelectInput(inputId = "fill", "Variable to group by:", shiny_data[,c(8,14:16,19,23,25,26,29)]),
                    selectInput("region", "Region of residence:", reg),
                    selectInput(inputId = "country", label = "Country of residence:", nat),
                    selectInput(inputId = "eureg", label = "Geographical region:", eureg))
                )),
      
      tabItem(tabName = "plots", h1("Bivariate line graph to customize"),
              fluidRow(
                box(title = "Line plot", status = "primary", solidHeader = T, width = 8,
                    plotOutput("lineplot")),
                box(title = "Controls for line plot", status = "warning", solidHeader = T, 
                    width = 4,
                    "Choose your variables for plotting, for singular countries or regions select from 'Country:' or 'Geographical region:'", br(),br(),
                    varSelectInput("xvar", "Variable on x-axis:", shiny_data[,1:30]),
                    varSelectInput("yvar", "Variable on y-axis:", shiny_data[,1:30]),
                    selectInput("region2", "Region of residence:", reg),
                    selectInput("country2", "Country of residence:", nat),
                    selectInput("eureg2", "Geographical region:", eureg))
              )),
      
      tabItem(tabName = "box", h1("Boxplots to customize"),
              fluidRow(
                box(title = "Boxplot", status = "primary", solidHeader = T, width = 8,
                    plotOutput("boxplot")),
                box(title = "Controls for boxplot", status = "warning", solidHeader = T, 
                    width = 4,
                    varSelectInput("group", "Plot grouped by (x-Axis):", shiny_data[,c(31:33)]),
                    varSelectInput("observ", "Variable to observe (y-Axis):", shiny_data[,1:30]),
                    varSelectInput("order", "Variable to order groups:", shiny_data[,34:39]))
              )),
      
      tabItem(tabName = "regress", h1("Linear, mixed-effects models to customize"),
              fluidRow(
                box(title = "Regression table", status = "primary", solidHeader = T, width = 8,
                    uiOutput("regtab")),
                box(title = "Controls for regression", status = "warning", solidHeader = T, 
                    width = 4,
                    varSelectInput("DV", "Dependent variable:", shiny_data[,1:30]),
                    varSelectInput("IDV", "Independent variable:", shiny_data[,1:30]),
                    checkboxGroupInput("mixreg", "Multilevel features:", selected = "None", 
                                       choices = c("None", "National level", "Regional level", "Both levels")),
                    varSelectInput("natvar", "Variables on national level:", shiny_data[,34:39]),
                    varSelectInput("regvar", "Variables on regional level:", shiny_data[,40:43]))
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
        coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
    }
  })
  
  output$regiomap <- renderPlot({})
  
  output$cdb <- renderText(includeText("EVS_final_cdbk"))
  
  output$down <- downloadHandler("EVS_final_cdbk.txt", "EVS_final_cdbk.txt")
  
  output$barchart <- renderPlot({
    if(!!input$country == "None" & !!input$eureg == "None" & !!input$region == "None"){
      if(!!input$check != T){
        ggplot(shiny_data, aes(x = !!input$variable))+
          geom_bar(color = "grey58", fill = "grey58")+
          #geom_text(x = paste(prop.table(input$variable)*100))+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                  summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      } else {
        ggplot(shiny_data, aes(x = !!input$variable, fill = !!input$fill))+
          geom_bar()+
          #geom_text(x = paste(prop.table(input$variable)*100))+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                         summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }
      
    }else if(!!input$country == "None" & !!input$eureg != "None" & !!input$region == "None"){
      if(!!input$check != T){
        shiny_data %>% 
          filter(Geographical_region == input$eureg) %>% 
          ggplot(aes( x = !!input$variable))+
          geom_bar(color = "grey58", fill = "grey58")+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                         filter(Geographical_region == input$eureg) %>%
                                                                         summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      } else {
        shiny_data %>% 
          filter(Geographical_region == input$eureg) %>% 
          ggplot(aes(x = !!input$variable, fill = !!input$fill))+
          geom_bar()+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                  filter(Geographical_region == input$eureg) %>%
                                                                  summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }
      
    }else if(!!input$country != "None" & !!input$eureg == "None" & !!input$region == "None"){
      if(!!input$check != T){
        shiny_data %>% 
          filter(Country_of_residence == input$country) %>% 
          ggplot(aes(x = !!input$variable))+
          geom_bar(color = "grey58", fill = "grey58")+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                  filter(Country_of_residence == input$country) %>% 
                                                                  summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }else {
        shiny_data %>% 
          filter(Country_of_residence == input$country) %>% 
          ggplot(aes(x = !!input$variable, fill = !!input$fill))+
          geom_bar()+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                         filter(Country_of_residence == input$country) %>% 
                                                                         summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }
      
    }else if(!!input$country == "None" & !!input$eureg == "None" & !!input$region != "None"){
      if(!!input$check != T){
        shiny_data %>% 
          filter(Region_of_residence == input$region) %>% 
          ggplot(aes(x = !!input$variable))+
          geom_bar(color = "grey58", fill = "grey58")+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                         filter(Region_of_residence == input$region) %>% 
                                                                         summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }else {
        shiny_data %>% 
          filter(Region_of_residence == input$country) %>% 
          ggplot(aes(x = !!input$variable, fill = !!input$fill))+
          geom_bar()+
          labs(x = paste(input$variable), caption = paste("n =", paste(shiny_data %>%
                                                                         filter(Region_of_residence == input$region) %>% 
                                                                         summarise(n()))))+
          theme_ipsum(grid = "Y") +
          coord_flip()
      }
      
    }else{
      text = paste("\n   You chose from two of 'Country_of_residence:',\n",
                   "    'Geographical_region:' or 'Region_of_residence'.\n",
                   "    Please do only select from one of those at a time.\n",
                   "         To deselect, put the dropdown on 'None'.")
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = text) + 
        theme_void() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
    }
  })
  
  output$lineplot <- renderPlot({
    if(!!input$country2 == "None" & !!input$eureg2 == "None" & !!input$region2 == "None"){
      ggplot(shiny_data, aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar), caption = paste("n =", 
                  paste(shiny_data %>% summarise(n()))))+
        theme_ipsum(grid = "Y")
      
    }else if(!!input$country2 == "None" & !!input$eureg2 != "None" & !!input$region2 == "None"){
      shiny_data %>% 
        filter(Geographical_region == input$eureg2) %>% 
        ggplot(aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar), caption = paste("n =", 
                  paste(shiny_data %>% filter(Geographical_region == input$eureg2) %>% 
                      summarise(n()))))+
        theme_ipsum(grid = "Y")
      
    }else if(!!input$country2 != "None" & !!input$eureg2 == "None" & !!input$region2 == "None"){
      shiny_data %>% 
        filter(Country_of_residence == input$country2) %>% 
        ggplot(aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar), caption = paste("n =", 
                  paste(shiny_data %>% filter(Country_of_residence == input$country2) %>% 
                      summarise(n()))))+
        theme_ipsum(grid = "Y")
      
    }else if(!!input$country2 == "None" & !!input$eureg2 == "None" & !!input$region2 != "None"){
      shiny_data %>% 
        filter(Region_of_residence == input$region2) %>% 
        ggplot(aes(x = !!input$xvar, y = !!input$yvar))+
        geom_jitter(alpha = 0.7, color = "grey58")+
        geom_smooth(method = "lm", size = 1.1)+
        labs(x = paste(input$xvar), y = paste(input$yvar), caption = paste("n =", 
                  paste(shiny_data %>% filter(Region_of_residence == input$region2) %>% 
                      summarise(n()))))+
        theme_ipsum(grid = "Y")
      
    }else{
      text = paste("\n   You chose from two of 'Country_of_residence:',\n",
                   "    'Geographical_region:' or 'Region_of_residence'.\n",
                   "    Please do only select from one of those at a time.\n",
                   "         To deselect, put the dropdown on 'None'.")
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
      labs(x = paste(input$group, "ordered after", input$order), y = paste(input$observ),
           caption = paste("n =", paste(shiny_data %>% summarise(n()))))
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
EVS_final %>% 
  select(reg) %>% 
  summarise(mean())
mean(EVS_final$reg, na.rm = T)

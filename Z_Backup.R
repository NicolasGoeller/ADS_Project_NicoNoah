### Stuff we have coded, but we couldn't implement in current workstream

## 4. Construct index for social capial
## Draw on initial EVS_2008 to obtain proxies for social capital 

# Our own social capital index with questionable intercorrelatedness:

EVS_2008 %<>% within({
  
  imp_fam <- v2 #importnace of family (=very important; 4=not at all)
  imp_fam[v2 %in% c(-5, -4, -3, -2, -1)] <- NA
  imp_fam <- (imp_fam-4)*-1
  imp_fam <- as.numeric(imp_fam)
  
  imp_frds <- v3 #importance of friends and acquaintances (=very important; 4=not at all) 
  imp_frds[v3 %in% c(-5, -4, -3, -2, -1)] <- NA
  imp_frds <- (imp_frds-4)*-1
  imp_frds <- as.numeric(imp_frds)
  
  trust <- v62 #people can be trusted/cant be too careful (dummy: 1=trusted; 2=be careful)
  trust[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  fair <- v63 #people try to take advantage or are fair (1=advantage; 10=fair)
  fair[v63 %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  helpful <- v64 #people are helpful or look after themselves (1=look out for themselves; 10=helpful)
  helpful[v64 %in% c(-5, -4, -3, -2, -1)] <- NA
  helpful <- as.numeric(helpful)
  
  met_pep <- v97 #meeting nice people (1=very important; 4=not important at all)
  met_pep[v97 %in% c(-5, -4, -3, -2, -1)] <- NA
  met_pep <- (met_pep-4)*-1
  met_pep <- as.numeric(met_pep)
  
  conc_fam <- v284 #concerned with familiy (1=very much; 5 not at all)
  conc_fam[v284 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_fam <- (conc_fam-5)*-1
  conc_fam <- as.numeric(conc_fam)
  
  conc_neigh <- v285 #concerned with people in neighbourhood (1=very much; 5 not at all)
  conc_neigh[v285 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_neigh <- (conc_neigh-5)*-1
  conc_neigh <- as.numeric(conc_neigh)
  
  conc_region <- v286 #concerned with people in region (1=very much; 5 not at all)
  conc_region[v286 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_region <- (conc_region-5)*-1
  conc_region <- as.numeric(conc_region)
  
})

#Construct social capital dataset
soc_cap_data <- select(EVS_2008, imp_fam,
                       imp_frds,
                       #fair,
                       #helpful,
                       met_pep, 
                       conc_fam,
                       conc_neigh,
                       conc_region)
soc_cap_data %<>% na.omit()
cor_mat <- cor(soc_cap_data) %>% round(2) #get correlation matrix
soc_cap_data %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 


###Shiny
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    #generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
})


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

menuItem("Introduction", tabName = "intro", icon = icon("info-sign", lib = "glyphicon")),

vis_data <- dplyr::select(EVS_final, sat, edu_cat, nation, eureg, siops)
vis_data %<>% na.omit

shiny_data <- list(
  "Life satisfaction" = vis_data$sat,
  "Educational categories" = vis_data$edu_cat,
  "Country of residence" = vis_data$nation,
  "Geographical region" = vis_data$eureg,
  "SIOPS-index" = vis_data$siops
)

nat_geodata <- read_rds("Data/Nation_geoData.rds")

output$map <- renderPlot({
  ggplot(data = nat_geodata)+
    geom_sf(aes(fill = !!input$macro))+
    labs(fill = paste(input$macro))+
    coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
  
  box(title = "Europe map plot", status = "primary", solidHeader = T, width = 8,
      plotOutput("map")),
  box(title = "Controls for map plot", status = "warning", solidHeader = T, 
      width = 4,
      "Choose your variable for plotting",br(), br(),
      varSelectInput("macro", label = "Variable:", nat_geodata))

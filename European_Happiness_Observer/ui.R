#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#
install.packages("shiny")
install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

# Define UI for application
shiny::shinyUI(
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
  )
)


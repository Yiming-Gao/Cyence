#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Scores"),
  
  # Select from cyence score, susceptibility or motivation
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      
       selectInput(inputId = "var", 
                   label = "Variable:",
                   choices = c("Cyence Score" = "cy", "Susceptibility" = "sus", "Motivation" = "mo"),
                   selected = NULL)
    ),
    
    # Show a plot 
    mainPanel(
       plotOutput("plot")
    )
  )
))

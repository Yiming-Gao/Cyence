#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("Changes by Cyence Sector"),
    
    # generate a row with a sidebar
    sidebarLayout(
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("sector", "Sector: ",
                    choices = c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                                "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                                "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                                "Transportation Services", "Agriculture & Mining")),
        hr(),
        helpText("Data from lastest month")
      ),
      
      # create a spot for time series plots
      mainPanel(
        plotlyOutput("top15", height = "800px")
      )
    )
  )
)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)

shinyUI(navbarPage(theme = shinytheme("flatly"), "Monthly Visualizations of Cyence Scores",
                   # first tab panel (with two sub-panels)
                   navbarMenu("US",
                              # first sub-panel
                              tabPanel("Map",
                                       fluidPage(
                                         title = "Map",
                                         leafletOutput("map", height = "800px")
                                       )
                              ),
                              
                              # second sub-panel
                              tabPanel("HeatMap",
                                       fluidPage(
                                         title = "Heatmap",
                                         plotlyOutput("heatmap", height = "800px"),
                                         h4(textOutput("legend_explanation"))
                                       )),
                              
                              # third sub-panel
                              # generate a row with a sidebar
                              tabPanel("Companies by Sector",
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
                                             selectInput("revenue_bin", "Revenue bin: ",
                                                         choices = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")), 
                                             hr(),
                                             helpText("Data from latest month"),
                                             hr(),
                                             checkboxInput("gap_or_not", "Include companies in GAP", value = FALSE),
                                             hr(),
                                             helpText("Notice that some companies only have two months' data (which were potentially in GAP)")
                                           ),
                                           
                                           # create a spot for time series plots
                                           mainPanel(
                                             h4(textOutput("text_revenue_bin")),
                                             plotlyOutput("top10", height = "1000px")
                                           )
                                         )
                                       ))
                   ),
                   
                   # second tab panel
                   tabPanel("EU"),
                   
                   # third tab panel
                   tabPanel("JP")
))

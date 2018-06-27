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

shinyUI(navbarPage(theme = shinytheme("readable"), "Monthly Visualizations of Cyence Scores",
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
                                         sidebarLayout(
                                           sidebarPanel(
                                             radioButtons("score_type", "Score Type",
                                                          c("Cyence rating" = "cy",
                                                            "Susceptibility" = "sus",
                                                            "Motivation" = "mo"),
                                                          selected = NULL),
                                             width = 4 # sidebar width
                                           ),
                                           
                                           mainPanel(
                                             plotlyOutput("heatmap", height = "800px"),
                                             h5(textOutput("legend_explanation"))
                                           )
                                         )
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
                                                                     "Transportation Services", "Agriculture & Mining"),
                                                         selected = "Wholesale Trade"),
                                             selectInput("revenue_bin", "Revenue bin: ",
                                                         choices = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"),
                                                         selected = "50-100M"), 
                                             hr(),
                                             helpText("Data from latest month"),
                                             hr(),
                                             checkboxInput("gap_or_not", "Include companies in GAP", value = FALSE)
                                           ),
                                           
                                           # create a spot for time series plots
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel("Time Series",
                                                        h5(textOutput("text_revenue_bin")),
                                                        plotlyOutput("top10", height = "1000px")),
                                               tabPanel("Normalized Scores",
                                                        h5(textOutput("text_diverging")),
                                                        plotlyOutput("diverging", height = "1000px"))
                                             )
                                           )
                                         )
                                       ))
                   ),
                   
                   # second tab panel
                   tabPanel("EU"),
                   
                   # third tab panel
                   tabPanel("JP")
))

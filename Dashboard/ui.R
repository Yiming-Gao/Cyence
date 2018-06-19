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

shinyUI(navbarPage("Monthly Visualizations of Cyence Scores",
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
                                plotlyOutput("heatmap", height = "800px")
                              ))
                   ),
                   
                   # second tab panel
                   tabPanel("EU"),
                   
                   # third tab panel
                   tabPanel("JP")
                   ))

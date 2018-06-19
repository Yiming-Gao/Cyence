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
                   # first tab panel
                   tabPanel("US",
                            sidebarLayout(
                              sidebarPanel(
                                "Map"
                              ),
                              
                              mainPanel(
                                leafletOutput("map"), height = 600
                              )
                            ))
                   ))

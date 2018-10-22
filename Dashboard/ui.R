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
library(plotly)

shinyUI(navbarPage(theme = shinytheme("readable"), "Monthly Visualizations of Cyence Scores",
                   ########## first tab panel (with two sub-panels)
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
                                                         choices = c("All", sort(c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                                                                                   "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                                                                                   "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                                                                                   "Transportation Services", "Agriculture & Mining"))),
                                                         selected = "All"),
                                             
                                             selectInput("revenue_bin", "Revenue bin: ",
                                                         choices = c("All", "0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"),
                                                         selected = "All"), 
                                             hr(),
                                             checkboxInput("gap_or_not", "Include companies in GAP", value = FALSE),
                                             checkboxInput("same_y_scale_or_not", "Force same scale for y-axis (only applicable to time series)", value = FALSE),
                                             hr(),
                                             helpText("The data is from latest month.")
                                           ),
                                           
                                           # create a spot for time series plots
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel("Time Series",
                                                        h5(textOutput("text_revenue_bin")),
                                                        plotlyOutput("top10", height = "1200px")),
                                               tabPanel("Normalized Scores",
                                                        h5(textOutput("text_diverging")),
                                                        plotlyOutput("diverging", height = "1000px"))
                                             )
                                           )
                                         )
                                       ))
                   ),
                   
                   
                   
                   
                   ########## second tab panel
                   navbarMenu("EU",
                              # first sub-panel
                              tabPanel("Map",
                                       fluidPage(
                                         title = "Map",
                                         leafletOutput("map_eu", height = "800px")
                                       )
                              ),
                              
                              # second sub-panel
                              tabPanel("HeatMap",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             radioButtons("score_type_eu", "Score Type",
                                                          c("Cyence rating" = "cy",
                                                            "Susceptibility" = "sus",
                                                            "Motivation" = "mo"),
                                                          selected = NULL),
                                             
                                             hr(),
                                             helpText("Notice that there might not be enough data for EU schema."),
                                             
                                             width = 4 # sidebar width
                                           ),
                                           
                                           mainPanel(
                                             plotlyOutput("heatmap_eu", height = "800px"),
                                             h5(textOutput("legend_explanation_eu"))
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
                                             selectInput("sector_eu", "Sector: ",
                                                         choices = c("All", sort(c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                                                                                   "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                                                                                   "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                                                                                   "Transportation Services", "Agriculture & Mining"))),
                                                         selected = "All"),
                                             
                                             selectInput("revenue_bin_eu", "Revenue bin: ",
                                                         choices = c("All","0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"),
                                                         selected = "All"), 
                                             hr(),
                                             checkboxInput("gap_or_not_eu", "Include companies in GAP", value = FALSE),
                                             checkboxInput("same_y_scale_or_not_eu", "Force same scale for y-axis (only applicable to time series)", value = FALSE),
                                             hr(),
                                             helpText("The data is from latest month.")
                                           ),
                                           
                                           # create a spot for time series plots
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel("Time Series",
                                                        h5(textOutput("text_revenue_bin_eu")),
                                                        plotlyOutput("top10_eu", height = "1200px")),
                                               tabPanel("Normalized Scores",
                                                        h5(textOutput("text_diverging_eu")),
                                                        plotlyOutput("diverging_eu", height = "1000px"))
                                             )
                                           )
                                         )
                                       ))
                   ),
                   
                   
                   
                   ########### third tab panel
                   navbarMenu("JP",
                              # first sub-panel
                              tabPanel("HeatMap",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             radioButtons("score_type_jp", "Score Type",
                                                          c("Cyence rating" = "cy",
                                                            "Susceptibility" = "sus",
                                                            "Motivation" = "mo"),
                                                          selected = NULL),
                                             
                                             hr(),
                                             helpText("Notice that there might not be enough data for JP schema."),
                                             
                                             width = 4 # sidebar width
                                           ),
                                           
                                           mainPanel(
                                             plotlyOutput("heatmap_jp", height = "800px"),
                                             h5(textOutput("legend_explanation_jp"))
                                           )
                                         )
                                       )),
                              
                              # second sub-panel
                              # generate a row with a sidebar
                              tabPanel("Companies by Sector",
                                       fluidPage(
                                         titlePanel("Changes by Cyence Sector"),
                                         
                                         # generate a row with a sidebar
                                         sidebarLayout(
                                           # Define the sidebar with one input
                                           sidebarPanel(
                                             selectInput("sector_jp", "Sector: ",
                                                         choices = c("All", sort(c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                                                                                   "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                                                                                   "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                                                                                   "Transportation Services", "Agriculture & Mining"))),
                                                         selected = "All"),
                                             
                                             selectInput("revenue_bin_jp", "Revenue bin: ",
                                                         choices = c("All","0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"),
                                                         selected = "All"), 
                                             hr(),
                                             checkboxInput("gap_or_not_jp", "Include companies in GAP", value = FALSE),
                                             checkboxInput("same_y_scale_or_not_jp", "Force same scale for y-axis (only applicable to time series)", value = FALSE),
                                             hr(),
                                             helpText("The data is from latest month.")
                                           ),
                                           
                                           # create a spot for time series plots
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel("Time Series",
                                                        h5(textOutput("text_revenue_bin_jp")),
                                                        plotlyOutput("top10_jp", height = "1200px")),
                                               tabPanel("Normalized Scores",
                                                        h5(textOutput("text_diverging_jp")),
                                                        plotlyOutput("diverging_jp", height = "1000px"))
                                             )
                                           )
                                         )
                                       ))
                   ),
                   
                   ########### fourth tab panel
                   # navbarMenu("Share Table - Software",
                   #            # first sub-panel
                   #            tabPanel("Some plots"),
                   #            
                   #            # second sub-panel
                   #            # generate a row with a sidebar
                   #            tabPanel("Coverage Changes",
                   #                     fluidPage(
                   #                       titlePanel("Coverage increase before and after smoothing"),
                   #                       
                   #                       # generate a row with a sidebar
                   #                       sidebarLayout(
                   #                         # Define the sidebar with one input
                   #                         sidebarPanel(
                   #                           selectInput("product", "Choose product: ",
                   #                                       choices = c(sort(all_sw$label), selected = "Apache HTTP Server"),
                   #                           hr(),
                   #                           helpText("The data is from latest month.")
                   #                         ),
                   #                         
                   #                         # output plots from share_table_coverage_comparison folder
                   #                         mainPanel(
                   #                           imageOutput("sw_st_coverage", height = "800px")
                   #                           # h5(textOutput("legend_explanation_jp"))
                   #                         )
                   #                       )
                   #                     ))
                   # )),
                   
                   
                   
                   
                   ########### fifth tab panel
                   tabPanel("About", 
                            "This dashboard is updated monthly.",
                            br(),
                            br(),
                            "Please visit",
                            a("this site", href = "https://cyence.atlassian.net/wiki/spaces/DS/overview"),
                            "for more information on data collection and aggregation.",
                            br(),
                            br(),
                            "Any questions or comments can be sent to",
                            br(),
                            "Mia Gao: ",
                            a("ygao@guidewire.com", href = "mailto:ygao@guidewire.com"))
))

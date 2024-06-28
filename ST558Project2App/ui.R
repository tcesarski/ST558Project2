#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Countries Project"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Data Download", tabName = "download", icon = icon("th")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Countries of the World",
                  img(src = 'worldflags.png', height = 300, width = 300)
                ),
                box(
                  title = "About This App",
                  "This application allows you to explore data related to countries and create plots, contingency tables, and numeric summaries on different countries individually, regions, subregions, or countries that speak a particular language. The data comes from the REST Countries API. More data about the REST Countries API and its endpoints can be found here:",
                  tags$a(href = "https://gitlab.com/restcountries/restcountries", "REST Countries API")
                  ),
                box(
                    title = "Purpose of Tabs",
                    "There are two additional tabs in this app: The Data Downlaod tab and the Data Exploration tab. The Data Download tab will allow you to specify changes to the data you want and display the data selected. You can also save this data as a file. The Data Exploration tab will allow you to produce graphical and numerical summaries based on this data.")
              )
            ),
      tabItem(tabName = "download",
              fluidRow(
                box(title = "Choose Filtering Method",
                    selectInput("filter", "Filter by:", 
                                choices = c("Country Name", "Region", "Language", "Population", "Area"))),
                box(title = "Select Columns",
                    checkboxGroupInput("cols", "Columns:",
                                       choices = c("Capital", "Region", "Subregion",
                                       "Area", "Population", "Car_Side_Driving",
                                       "Independence", "Landlocked", "UN_Member"))
                ),
                uiOutput("filter")
              ),
              fluidRow(
              downloadButton("download", "Download Data"),
              dataTableOutput("filtered_data")
              )
            ),
      tabItem(tabName = "explore",
              fluidRow(title = "Choose Summary Type",
                    selectInput("summary", "Summary Type:",
                                choices = c("Contingency Tables", 
                                            "Numerical Summaries",
                                            "Graphical Displays")),
                uiOutput("summary_out"),
                uiOutput("treemap_opt"),
                uiOutput("barchart_opt"),
                uiOutput("hist_opt"),
                uiOutput("cat1"),
                uiOutput("cat2"),
                uiOutput("cat1_table"),
                uiOutput("cat2_table"),
                uiOutput("con1_opt"),
                uiOutput("con2_opt")),
                uiOutput("facet_var"),
                uiOutput("scatter_opt"),
                tableOutput("contingency_tab"),
                tableOutput("contingency_two_tab"),
              conditionalPanel(
                condition = "input.summary" == "Graphical Displays",
                plotOutput("tree_graph"),
                plotOutput("bar_graph"),
                plotOutput("facet_histogram"),
                plotOutput("facet_scatter")
              )
              )
              
                )
              )
      )













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
#Set up dashboard page
ui <- dashboardPage(
  #Title as "Countries Project"
  dashboardHeader(title = "Countries Project"),
  #Create a sidebar with the tabs. Name tabs as "About", "Data Download", and "Data Exploration". Icons were found on fontawesome.
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("globe")),
      menuItem("Data Download", tabName = "download", icon = icon("download")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
  #In the "About" Tab, have a box called with an image and title it "Countries of the World.", Have another box with about this app and another with the purpose of the tabs.
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
                    "There are two additional tabs in this app: The Data Downlaod tab and the Data Exploration tab. The Data Download tab will allow you to specify changes to the data you want and display the data selected. You can also save this data as a file. The Data Exploration tab will allow you to produce graphical and numerical summaries based on this data."),
                box(
                  title = "Information about the Variables",
                  "Regions and Subregions are defined by the United Nations geoscheme which divides the world into 6 regions and 22 subregions. Languages includes all languages for that country. These are not mutually exclusive - a country can have more than one official language. Therefore when searching by language a country could come up in more than one language category. Area is reported in square miles. UN_Member is if they are a member of the United Nations. Car_Side_Driving is which side of the street they drive on. Note: There are no subregions for the Antarctic region so the table will not generate if subregion is selected."),
                )
              ),
    #In the download tab have two fluid rows of 6 & 6 (add to 12). Have a dropdown box to choose the method to filter by. Internal name is filter and visual name is "Filter by:".
      tabItem(tabName = "download",
              fluidRow(
                column(6,
                box(title = "Choose Filtering Method",
                    selectInput("filter", "Filter by:", 
                                choices = c("Country Name", "Region", "Language", "Population", "Area")))),
  #In the next box, have a checkbox where the user can check as many boxes as wanted and store those as internal name of cols and display name of "Columns:".
                column(6, 
                box(title = "Select Columns",
                    checkboxGroupInput("cols", "Columns:",
                                       choices = c("Capital", "Region", "Subregion", "Area", "Population", "Car_Side_Driving", "Independence", "Landlocked", "UN_Member")))
                ),
  #uiOutput from renderUI in the server file. This is where the user selected what the country name, etc. from the secondary dropdown box.
                uiOutput("choice")
              ),
  #Add in a download button.
              fluidRow(
              downloadButton("download", "Download Data"),
              dataTableOutput("data_table")
              )
            ),
  #Next tab is internally called explore.
      tabItem(tabName = "explore",
              #Use sidebarLayout with sidebarPanel and mainPanel so that it doesn't all stack on top of each other.
              sidebarLayout(
                sidebarPanel(
                  #Allow user to select what type of summary they want.
                  selectInput("summary", "Summary Type:",
                              choices = c("Contingency Tables", "Numerical Summaries", "Graphical Displays")),
                  #Generate uiOutputs from renderUI in server file.
                  uiOutput("summary_out"),
                  uiOutput("treemap_opt"),
                  uiOutput("barchart_opt"),
                  uiOutput("hist_opt"),
                  uiOutput("con1_opt"),
                  uiOutput("con2_opt"),
                  uiOutput("scatter_opt")
                ),
                #In main panel, output tables from renderTables in server file and output plots from renderPlots.
                mainPanel(
                  tableOutput("contingency_tab"),
                  tableOutput("contingency_two_tab"),
                  tableOutput("numeric_sum"),
                  conditionalPanel("input.graph == 'Tree Map'",
                                   plotOutput("tree_graph")),
                  conditionalPanel("input.graph== 'Bar Chart'",
                                   plotOutput("bar_graph")),
                  conditionalPanel("input.graph == 'Histograms'",
                                   plotOutput("facet_histogram")),
                  conditionalPanel("input.graph == 'Scatterplot'",
                                   plotOutput("facet_scatter"))
                )
              )
      )
    )
  )
)












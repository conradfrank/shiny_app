library(shiny)
library(shinythemes)
library(WDI)
library(plotly)

# Generating list of countries from WDI package
list1 <- WDI_data$country[,3]
shinyUI(navbarPage(theme = shinytheme("spacelab"),
                   "GDP data and Forecasting App",
  tabPanel("Intro", 
           hr(),
           p("In this app, there are 2 tabs."),
           p('First tab -',strong('GDP'),'- you can select from the list, a country whose GDP per capita(current US$) will be displayed from 1990 to 2017.'),
           p('Second tab -', strong('Forecast'), '- for the same country selected,'),
           p("you can now select the number of years between 2018 to 2028 to forecast the GDP"),
           br(),br(),br(),br(),br(),br(),br(),
           p("The data is derived from World Bank,'@' 2018 The World Bank Group, All Rights Reserved."),
           br(),br(),
           h4("Thank You for trying the app.")
          ),    

  tabPanel("GDP",
           sidebarPanel(
             selectInput("select", label = h4("Select Country name only "), 
                         choices = list1, selected = NULL,
                         multiple = FALSE,selectize = TRUE),
             actionButton("button1", "Plot")
           ),
           mainPanel(
             textOutput("text1"),
             plotlyOutput("plot1"))
           ),
  tabPanel("Forecast",
           sidebarPanel(
             h4("Select No. of years to Forecast"),
             sliderInput("slider1", "Select Year", 2018, 2028, 2018),
             actionButton("button2", "Plot")
           ),
           mainPanel(
             textOutput("text2"),
             plotlyOutput("plot2"))
           )
  
))

  

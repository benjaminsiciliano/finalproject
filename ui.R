library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Property Prices", tabName = "prices", icon = icon("dollar-sign")),
        menuItem("Property Demand", tabName = "demand", icon = icon("home")),
        menuItem("Explore the Raw Data", tabName = "data", icon = icon("table")),
        menuItem("Source & Acknowledgments", tabName = "source", icon = icon("flag")),
        div(""),
        div("Note: this web app is optimized for full"),
        div("screen view. Please maximize your"),
        div("browser window for the best DataViz"),
        div("experience!")
    )
)

body <- dashboardBody(
    tabItems(
         
        tabItem(tabName = "demand",
                h2("Exploring Property Demand in NYC"),
                fluidRow(
                    box(
                        title = "What are the City's Most Popular Nieghborhoods?", status = "warning", solidHeader = TRUE,
                        plotOutput("popularneighborhoods", height = 275)
                    ),
                    
                    box(
                        title = "Most Popular Neighborhoods: Sort by Borough", status = "warning", solidHeader = TRUE,
                        radioButtons("pnoptions",
                                     "Find most popular neighborhoods for:",
                                     choices = list("Whole City"="city", "By Borough"="boro")),
                        conditionalPanel(condition="input.pnoptions=='boro'",
                                         selectInput("borough", "Select Borough:",
                                                     choices = "", multiple = F)),
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        title = "Breakdown of Total Property Sales by Borough", status = "info", width = 4, solidHeader = TRUE,
                        plotlyOutput("demandByBorough", height = 275)
                    ),
                    box(
                        title = "Residential Haven or Commercialized Strip?", status = "primary", width = 8, solidHeader = TRUE,
                        plotOutput("propertytype", height = 275)
                    )
                )
        ),
        
        tabItem(tabName = "prices",
                h2("Exploring Property Pricing in NYC"),
                
                fluidRow(
                    box(
                        title = "Predicting Property Price", status = "primary", solidHeader = TRUE,
                        plotOutput("pricepredictors", height = 275)
                    ),
                    box(
                        title = "Explore Different Pricing Relationships", status = "primary", solidHeader = TRUE,
                        selectInput("predictor",
                                    "Predict Price by:",
                                    choices = c("Property Square Footage"='LAND SQUARE FEET', "Year Building Was Built"='YEAR BUILT')),
                        radioButtons("ppoptions",
                                     "Predict Price for:",
                                     choices = list("Whole City"="cityy", "By Borough"="boroo")),
                        conditionalPanel(condition="input.ppoptions=='boroo'",
                                         checkboxGroupInput("boroos",
                                                            "Select Borough(s) to Plot:",
                                                            choices = "")),
                        checkboxInput("smooth", "Add trend line"),
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        title = "What's the Most Expensive Borough?", status = "info", width = 4, solidHeader = TRUE,
                        plotOutput("pricebyborough", height=275)
                    ),
                    box(
                        title = "Compare the Affordability of Various Neighborhoods", status = "warning", width = 8, solidHeader = TRUE,
                        selectInput("selectneighborhoods",
                                    "Select Neighborhoods to Compare:",
                                    choices = "", multiple = TRUE),
                        plotOutput("neighborhoodprices", height=195)
                    )
                )
        ),
        tabItem(tabName = "data",
                h2("Explore the Raw Data"),
                
                DTOutput("mydata")
                
                ),
        tabItem(tabName = "source",
                h2("Data Source & Acknowledgements"),
                
                div(class = "my-class", p("This web app visualizes data from the dataset 'NYC Property Sales', which details real estate transactions that took place in the five boroughs of New York City in 2016. The original dataset was published by the City of New York, and can be viewed and downloaded at https://www.kaggle.com/new-york-city/nyc-property-sales.")),
                div(class = "my-class", p("The preparation and cleaning of data for this project were facilitated by the great help of Bernhard Klingenberg, Professor of Statistics at Williams College. Some data preparation techniques were also inspired by the work of Preethi Jayaraman, who also worked with this dataset in a project that can be found at https://rstudio-pubs-static.s3.amazonaws.com/363900_342aa40e88404e2eaf9dbf95163c1cc6.html."))
        )
    )
)

dashboardPage(
    skin="yellow",
    dashboardHeader(title = "Exploring NYC Property"),
    sidebar,
    body
)
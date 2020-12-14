library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Housing Demand", tabName = "demand", icon = icon("dashboard")),
        menuItem("Housing Prices", tabName = "prices", icon = icon("dashboard")),
        menuItem("Explore the Raw Data", tabName = "data", icon = icon("dashboard")),
        div(""),
        div("Note: this web app is optimized for full "),
        div("screen view. Please maximize your"),
        div("browser window for the best DataViz"),
        div("experience!")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "demand",
                h2("Exploring Housing Demand in NYC"),
                fluidRow(
                    box(
                        title = "Where Are Property Sales the Highest?", status = "primary", solidHeader = TRUE,
                        plotOutput("popularneighborhoods", height = 275)
                    ),
                    
                    box(
                        title = "Sort by Borough", status = "warning", solidHeader = TRUE,
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
                        title = "Breakdown of Property Sales by Borough", status = "primary", width = 4, solidHeader = TRUE,
                        plotlyOutput("demandByBorough", height = 275)
                    ),
                    box(
                        title = "Types of Property Demanded, by Borough", status = "warning", width = 4, solidHeader = TRUE
                    ),
                    box(
                        title = "This is the Third box", status = "primary", width = 4, solidHeader = TRUE
                    )
                )
        ),
        
        tabItem(tabName = "prices",
                h2("Exploring Housing Pricing in NYC"),
                fluidRow(
                    box(
                        title = "Explore Differet Pricing Relationships by Borough", status = "primary", solidHeader = TRUE,
                        plotOutput("pricepredictors", height = 275)
                    ),
                    box(
                        title = "Inputs", status = "warning", solidHeader = TRUE,
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
                        title = "What's the most Expensive Borough", status = "primary", width = 4, solidHeader = TRUE,
                        plotOutput("pricebyborough", height=275)
                    ),
                    box(
                        title = "Types of Property Demanded, by Borough", status = "warning", width = 8, solidHeader = TRUE
                    )
                )
        ),
        tabItem(tabName = "data",
                h2("Explore the Raw Data"))
    )
)

dashboardPage(
    skin="yellow",
    dashboardHeader(title = "Exploring NYC Housing Data"),
    sidebar,
    body
)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

mypath <- './data/nyc-rolling-sales.csv'
nycdata <- read_csv(mypath) %>%
  mutate(BOROUGH = as.factor(BOROUGH)) %>%
  mutate(BOROUGH = fct_recode(BOROUGH, "Manhattan"="1", "The Bronx"="2", "Brooklyn"="3", "Queens"="4", "Staten Island"="5"))

uniqueboroughs <- unique(nycdata$BOROUGH)

shinyServer(function(input, output, session) {
  
  observeEvent(input$pnoptions, {
    mychoices = uniqueboroughs
    print(str(nycdata))
    updateSelectInput(session, "borough", choices = mychoices, selected = "Manhattan")
  },
  once = T)
  
  observeEvent(input$ppoptions, {
    mychoices1 = uniqueboroughs
    updateCheckboxGroupInput(session, "boroos", choices = mychoices1, selected = "Manhattan")
  },
  once = T)
  
  output$popularneighborhoods <- renderPlot({
    
    if(input$pnoptions=="boro") nycdata <- nycdata %>% filter(BOROUGH %in% input$borough)
    else nycdata <- nycdata
    
    nycdata_neighborhood <- nycdata %>% 
      mutate(NEIGHBORHOOD1=fct_lump(NEIGHBORHOOD, n = 10)) %>%
      count(NEIGHBORHOOD1) %>%
      mutate(NEIGHBORHOOD1=fct_reorder(NEIGHBORHOOD1, n)) %>%
      filter(NEIGHBORHOOD1 != "Other")
    p <- ggplot(data = nycdata_neighborhood, aes(x = NEIGHBORHOOD1, y = n)) +
      geom_bar(stat='identity', fill="#FF7F00", color = "blue") +
      coord_flip(ylim=c(0, 3500))
    
    if (input$pnoptions == "city") p <- p + labs(x="Neighborhood", 
                                                 y ="Number of Property Sales (2016)",
                                                 title = "Ten Most Popular Neighborhoods in the Whole City") +
      scale_y_continuous(expand = c(0,0)) +
      theme(plot.title = element_text(hjust=0.5)) +
      theme_classic()
    if (input$pnoptions == "boro") p <- p + labs(x="Neighborhood", 
                                                 y ="Number of Property Sales (2016)",
                                                 title = "Ten Most Popular Neighborhoods in the Borough") +
      scale_y_continuous(expand = c(0,0)) +
      theme(plot.title = element_text(hjust=0.5)) +
      theme_classic()
    
    return(p)
  })
  
  output$pricepredictors <- renderPlot({
    ggplot(data = nycdata, aes(x = 'input$predictor', y = 'SALE PRICE')) + geom_point()
  })
  
})

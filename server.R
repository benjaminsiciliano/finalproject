library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

mypath <- './data/nyc_rolling_sales1.csv'
nycdata <- read_csv(mypath) %>%
  mutate(BOROUGH = as.factor(BOROUGH)) %>%
  mutate(BOROUGH = fct_recode(BOROUGH, "Manhattan"="1", "The Bronx"="2", "Brooklyn"="3", "Queens"="4", "Staten Island"="5")) %>%
  filter(nycdata$'YEAR BUILT' >= 1500)
nycdata <- nycdata %>% filter(nycdata$'SALE PRICE' >= 100)
  
uniqueboroughs <- unique(nycdata$BOROUGH)

ntotal <- nrow(nycdata)
ntotal

shinyServer(function(input, output, session) {
  print(min(nycdata$'YEAR BUILT'))
  print(min(nycdata$'SALE PRICE'))
  
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
      coord_flip(ylim=c(0, 2500))
    
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
  
  output$demandByBorough <- renderPlotly({
    
    boroughbreakdown <- nycdata %>% group_by(BOROUGH) %>% count() %>% mutate(prop=(n/ntotal)*100)
    
    piechart <- plot_ly(
      data = boroughbreakdown,
      labels = ~BOROUGH,
      values = ~prop,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label',
      text = ~round(prop, digits = 1),
      hovertemplate = paste('<b>%{text}%</b> <br>'),
      hoverinfo = 'text',
      showlegend = FALSE
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    return(piechart)
  })
  
  output$pricepredictors <- renderPlot({
    
    if(input$ppoptions=="boroo") nycdata <- nycdata %>% filter(BOROUGH %in% input$boroos)
      else nycdata <- nycdata
    
    options(scipen=999)
    p_land_city <- ggplot(data = nycdata, aes(x = log(.data[[input$predictor]]), y = log(.data[['SALE PRICE']]))) + 
      geom_point(alpha=0.5) +
      labs(x = 'Property Square Footage, log scale',
           y = 'Property Sale Price (USD), log scale') +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) 
    p_land_borough <- ggplot(data = nycdata, aes(x = log(.data[[input$predictor]]), y = log(.data[['SALE PRICE']]), color = BOROUGH)) + 
      geom_point(alpha=0.5) +
      labs(x = 'Property Square Footage, log scale',
           y = 'Property Sale Price (USD), log scale') +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            legend.background = element_rect(fill="beige", size=0.5, linetype="solid", color = 'black'))
    p_year_city <- ggplot(data = nycdata, aes(x = .data[[input$predictor]], y = log(.data[['SALE PRICE']]))) + 
      geom_point(alpha=0.5) +
      labs(x = 'Year Building was Built',
           y = 'Property Sale Price (USD), log scale') +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5))
    p_year_borough <- ggplot(data = nycdata, aes(x = .data[[input$predictor]], y = log(.data[['SALE PRICE']]), color = BOROUGH)) + 
      geom_point(alpha=0.5) +
      labs(x = 'Year Building was Built',
           y = 'Property Sale Price (USD), log scale') +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            legend.background = element_rect(fill="beige", size=0.5, linetype="solid", color = 'black'))
    
    plot <- if(input$predictor == 'LAND SQUARE FEET' & input$ppoptions == 'cityy') p_land_city
              else if(input$predictor == 'LAND SQUARE FEET' & input$ppoptions == 'boroo') p_land_borough
               else if(input$predictor == 'YEAR BUILT' & input$ppoptions == 'cityy') p_year_city
                  else p_year_borough
    if(input$smooth) plot <- plot + geom_smooth(method='lm', se=FALSE, alpha=0.5)
      else plot <- plot
    
    return(plot)
    
  })
  
  output$pricebyborough <- renderPlot({
    
    nycdata <- nycdata %>% mutate(BOROUGH = fct_reorder(BOROUGH, 'SALE PRICE'))
    
    ggplot(data = nycdata, aes(x=BOROUGH, y = .data[['SALE PRICE']])) +
      geom_boxplot(fill="orange", outlier.shape = 21, outlier.fill = "blue", outlier.alpha = 0.3) +
      scale_y_log10() +
      coord_flip() +
      labs(x="", 
           y="Sale Price (log scale)",
           title="Brooklyn is Coming, but Manhattan's Still King") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5))
  })
  
})

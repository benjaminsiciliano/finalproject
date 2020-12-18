library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

mypath <- './data/nyc_rolling_sales1.csv'
nycdata <- read_csv(mypath) %>%
  mutate(BOROUGH = as.factor(BOROUGH)) %>%
  mutate(BOROUGH = fct_recode(BOROUGH, "Manhattan"="1", "The Bronx"="2", "Brooklyn"="3", "Queens"="4", "Staten Island"="5")) %>%
  filter(.data[['YEAR BUILT']] >= 1500)
  
uniqueboroughs <- unique(nycdata$BOROUGH)
uniqueneighborhoods <- unique(nycdata$NEIGHBORHOOD)

ntotal <- nrow(nycdata)
ntotal

shinyServer(function(input, output, session) {
  
  observeEvent(input$pnoptions, {
    mychoices = uniqueboroughs
    updateSelectInput(session, "borough", choices = mychoices, selected = "Manhattan")
  },
  once = T)
  
  observeEvent(input$ppoptions, {
    mychoices1 = uniqueboroughs
    updateCheckboxGroupInput(session, "boroos", choices = mychoices1, selected = "Manhattan")
  },
  once = T)
  
  mychoices2 = uniqueneighborhoods
  updateSelectInput(session, "selectneighborhoods", choices = mychoices2, selected = c("FOREST HILLS", "CHELSEA", "COBBLE HILL"))

  
  output$popularneighborhoods <- renderPlot({
    
    if(input$pnoptions=="boro") nycdata <- nycdata %>% filter(BOROUGH %in% input$borough)
      else nycdata <- nycdata
    
    nycdata_neighborhood <- nycdata %>% 
      mutate(NEIGHBORHOOD1=fct_lump(NEIGHBORHOOD, n = 10)) %>%
      group_by(BOROUGH) %>%
      count(NEIGHBORHOOD1) %>%
      mutate(NEIGHBORHOOD1=fct_reorder(NEIGHBORHOOD1, n)) %>%
      filter(NEIGHBORHOOD1 != "Other")
    
    if (input$pnoptions == "city") p <- ggplot(data = nycdata_neighborhood, aes(x = reorder(NEIGHBORHOOD1, n), y = n, fill=BOROUGH)) +
      geom_bar(stat='identity') +
      coord_flip(ylim=c(0, 3500)) + labs(x="", 
                                         y ="Number of Property Sales",
                                         title = "Neighborhoods with Most Property Sales in the City (2016)") +
      scale_y_continuous(expand = c(0,0)) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            legend.background = element_rect(fill="beige", size=0.5, linetype="solid", color = 'black'))
   
     if (input$pnoptions == "boro") p <- ggplot(data = nycdata_neighborhood, aes(x = reorder(NEIGHBORHOOD1, n), y = n)) +
      geom_bar(stat='identity', fill="#EE7621") +
      coord_flip(ylim=c(0, 3500)) + labs(x="", 
                                         y ="Number of Property Sales",
                                         title = "Neighborhoods with Most Property Sales in the Borough (2016)") +
      scale_y_continuous(expand = c(0,0)) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5))
    
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
      hovertemplate = paste('<b>%{text}%</b><extra></extra>'),
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
    
    nycdata <- nycdata %>% filter((.data[['SALE PRICE']] >= 10000) & !is.na(.data[['SALE PRICE']]))
    nycdata <- nycdata %>% filter(.data[['LAND SQUARE FEET']] >= 200)
    
    if(input$ppoptions=="boroo") nycdata <- nycdata %>% filter(BOROUGH %in% input$boroos)
      else nycdata <- nycdata
    
    options(scipen=999)
    p_land_city <- ggplot(data = nycdata, aes(x = log(.data[[input$predictor]]), y = log(.data[['SALE PRICE']]))) + 
      geom_point(alpha=0.5, color='orange') +
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
      geom_point(alpha=0.5, color='orange') +
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
    
    nycdata <- nycdata %>% filter((.data[['SALE PRICE']] >= 1000) & !is.na(.data[['SALE PRICE']]))
    
    ggplot(data = nycdata, aes(x=reorder(BOROUGH, .data[['SALE PRICE']], median), y = .data[['SALE PRICE']])) +
      geom_boxplot(fill="orange", outlier.shape = 21, outlier.fill = "blue", outlier.alpha = 0.3) +
      scale_y_log10() +
      coord_flip() +
      labs(x="", 
           y="Sale Price (log scale)",
           title="Brooklyn is Coming, but Manhattan's Still King",
           subtitle="Sale Price Distribution by Borough") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })

  output$neighborhoodprices <- renderPlot({
    
    options(scipen=999)
    
    nycdata <- nycdata %>% filter((.data[['SALE PRICE']] >= 1000) & !is.na(.data[['SALE PRICE']]))
    
    salepriceneighborhood_med <- nycdata %>% group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(mediansaleprice = median(.data[['SALE PRICE']]))
    
    salepriceneighborhood_med <- salepriceneighborhood_med %>% filter(NEIGHBORHOOD %in% input$selectneighborhoods)
    
    ggplot(data=salepriceneighborhood_med, aes(x=NEIGHBORHOOD, y=mediansaleprice, fill=BOROUGH)) +
      geom_bar(stat='identity') +
      coord_flip() +
      labs(x="", 
           y ="Median Sale Price, USD",
           title = "Median Sale Price of Property in Selected Neighborhood(s), 2016") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            legend.background = element_rect(fill="beige", size=0.5, linetype="solid", color = 'black')) +
      scale_y_continuous(expand=expansion(mult=c(0, 0.1)))
      

  })
  
  output$propertytype <- renderPlot({
    
    propertytypeplot <- nycdata %>% mutate(type=ifelse(nycdata$'BUILDING CLASS CATEGORY' %in% c("01 ONE FAMILY DWELLINGS", "02 TWO FAMILY DWELLINGS", "03 THREE FAMILY DWELLINGS", "07 RENTALS - WALKUP APARTMENTS", "08 RENTALS - ELEVATOR APARTMENTS", "09 COOPS - WALKUP APARTMENTS", "10 COOPS - ELEVATOR APARTMENTS", "11A CONDO-RENTALS", "12 CONDOS - WALKUP APARTMENTS", "13 CONDOS - ELEVATOR APARTMENTS", "14 RENTALS - 4-10 UNIT", "15 CONDOS - 2-10 UNIT RESIDENTIAL", "16 CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT", "17 CONDO COOPS"), "Residential", "Commercial/Other")) %>%
      group_by(BOROUGH) %>% count(type) %>% mutate(percent=100*(n/sum(n)), residential=ifelse(type=="Residential", percent, 0))
    
    ggplot(data=propertytypeplot, aes(x=reorder(BOROUGH, -residential), y=percent, fill=type)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(percent, digits = 1)), position = position_stack(vjust = 0.5), color = 'white', size = 5) +
      labs(title="Proportion of Property Transactions Involving Residential Buildings (2016)", 
           x="",
           y="Percentage") +
      scale_fill_manual(name="Building Type:", values=c('blue', 'orange')) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5),
            legend.background = element_rect(fill="beige", size=0.5, linetype="solid", color = 'black')) +
      scale_y_continuous(expand=expansion(mult=c(0, 0.1)))
  })
  
  output$mydata <- renderDT({
    
    nycdata <- nycdata %>% filter((.data[['SALE PRICE']] >= 1000) & !is.na(.data[['SALE PRICE']]))
    
    df <- nycdata %>% select(BOROUGH, NEIGHBORHOOD, ADDRESS, 'LAND SQUARE FEET', 'YEAR BUILT', 'SALE PRICE') %>%
      filter(!is.na(nycdata$'LAND SQUARE FEET'))
    
    return(df)
    
  
  })
  
})

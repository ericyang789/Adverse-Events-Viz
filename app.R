library(plotly)
library(shiny)
library(tidyverse)
library(crosstalk)

data <- read.csv('CAERS_ASCII_2004_2017Q2.csv')

# if no event start date, use report creation date
data$AEC_Event.Start.Date <- ifelse(data$AEC_Event.Start.Date=="", 
                                    data$RA_CAERS.Created.Date, 
                                    data$AEC_Event.Start.Date)

# group industries with smaller frequencies into "other"
top13 <- data %>%
  group_by(data$PRI_FDA.Industry.Name) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(13)
top13_names <- top13$`data$PRI_FDA.Industry.Name`
data$PRI_FDA.Industry.Name <- ifelse(data$PRI_FDA.Industry.Name %in% top13_names, 
                                     data$PRI_FDA.Industry.Name, 
                                     'Other')

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualization of Adverse Food Events"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # Show plots
    mainPanel(
      
    )
  )
)
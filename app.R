
library(plotly)
library(shiny)
library(tidyverse)
library(crosstalk)
library(lubridate)

# read and prepare the data

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
data$year <- year(mdy(data$AEC_Event.Start.Date))
data <- data %>%
  mutate(age_group = case_when(
    CI_Age.at.Adverse.Event <= 14 ~ 'Child (0-14 yrs)',
    CI_Age.at.Adverse.Event >= 15 & CI_Age.at.Adverse.Event <= 24 ~ 'Youth (15-24 yrs)',
    CI_Age.at.Adverse.Event >= 25 & CI_Age.at.Adverse.Event <= 64 ~ 'Adult (25-64 yrs)',
    CI_Age.at.Adverse.Event >= 65 ~ 'Senior (65+ yrs)',
    is.na(CI_Age.at.Adverse.Event) ~ 'Age Unknown'))


# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualization of Adverse Food Events"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = 'top_prod',
                  label = h3('Number of products to display:'),
                  min = 1, 
                  max = 25, 
                  value = 10, 
                  step = 1,
                  ticks = TRUE,
                  sep = ""),
      radioButtons(inputId = 'gender',
                   label = h3('Gender:'),
                   choices = c('Male', 'Female', 'All'),
                   selected = 'All'),
      sliderInput(inputId ='year',
                  label = h3('Year(s):'),
                  min = min(data$year),
                  max = max(data$year),
                  value = c(2000, 2017),
                  step = 1,
                  ticks = TRUE,
                  sep = ""),
      checkboxGroupInput(inputId ='age_grp',
                         label = h3('Age group(s):'),
                         choices = c('Child (0-14 yrs)','Youth (15-24 yrs)',
                                     'Adult (25-64 yrs)','Senior (65+ yrs)',
                                     'Age Unknown'),
                         selected = c('Child (0-14 yrs)','Youth (15-24 yrs)',
                                      'Adult (25-64 yrs)','Senior (65+ yrs)',
                                      'Age Unknown') 
      )
    ),
    
    # Show plots
    mainPanel(
      plotlyOutput("plot2"),
      verbatimTextOutput("click")
    )
  )
)

server <- function(input, output,session) {
  output$plot2 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender==c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group==input$age_grp)
    
    industry_year_grouped=filtered_data %>%
      group_by(PRI_FDA.Industry.Name,year) %>%
      summarise(n = n())
    
    plot_ly(industry_year_grouped,
            x = ~year, 
            y = ~n,
            type = 'scatter',
            mode= 'lines',
            color=~PRI_FDA.Industry.Name) %>% layout(yaxis = list(type = "log",title="log(Count)"),legend = list(font = list(size = 10)))
  })
  
  output$click <- renderPrint({
    # Problem 1.3, update the week slider here
    if (is.null(event_data("plotly_relayout"))) {
      "Zoom and Pan in the line chart also updates the slider"
    } else {
      #print(event_data("plotly_relayout"))
      invisible(updateSliderInput(inputId="year",
                                  value=c(event_data("plotly_relayout")$`xaxis.range[0]`,
                                          event_data("plotly_relayout")$`xaxis.range[1]`)))
    }
  });
  
  
  
}


shinyApp(ui, server, options = list(height=600))

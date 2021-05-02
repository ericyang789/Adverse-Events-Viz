library(plotly)
library(shiny)
library(tidyverse)
library(crosstalk)
library(lubridate)
library(stringr)
library(RColorBrewer)

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

# create age groups
data$year <- year(mdy(data$AEC_Event.Start.Date))
data <- data %>%
  mutate(age_group = case_when(
    CI_Age.at.Adverse.Event <= 14 ~ 'Child (0-14 yrs)',
    CI_Age.at.Adverse.Event >= 15 & CI_Age.at.Adverse.Event <= 24 ~ 'Youth (15-24 yrs)',
    CI_Age.at.Adverse.Event >= 25 & CI_Age.at.Adverse.Event <= 64 ~ 'Adult (25-64 yrs)',
    CI_Age.at.Adverse.Event >= 65 ~ 'Senior (65+ yrs)',
    is.na(CI_Age.at.Adverse.Event) ~ 'Age Unknown'))

# remove few samples prior to 2000
data <- data %>%
  filter(year >= 2000)

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
                  value = c(min(data$year), max(data$year)),
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
      plotlyOutput("plot1"),
      br(), br(),
      plotlyOutput("plot2"),
      #verbatimTextOutput("click"),
      br(), br(),
      plotlyOutput("plot3")
      
    )
  )
)

server <- function(input, output,session) {
  # create bar plot
  output$plot1 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender %in% c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group%in%input$age_grp)
    filtered_data$PRI_Reported.Brand.Product.Name <- str_to_title(filtered_data$PRI_Reported.Brand.Product.Name)
    product_grouped <- filtered_data %>%
      filter(filtered_data$PRI_Reported.Brand.Product.Name != 'Redacted') %>%
      group_by(PRI_Reported.Brand.Product.Name) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(input$top_prod)
    
    plot_ly(product_grouped,
            x = ~PRI_Reported.Brand.Product.Name, 
            y = ~n,
            type = 'bar') %>% 
      layout(title = list(text='Top Adverse Event Causing Products'),
             yaxis = list(title="Count"),
             xaxis=list(title="", categoryorder = "array", categoryarray = ~PRI_Reported.Brand.Product.Name,
                        ticks = 'outside',
                        tickfont = list(size = 10),
                        tickangle = 15, size = 5,
                        tickprefix=" "))  
  })
  
  output$plot2 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender %in% c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group%in%input$age_grp)
    
    industry_year_grouped=filtered_data %>%
      group_by(PRI_FDA.Industry.Name,year) %>%
      summarise(n = n())
    
    plot_ly(industry_year_grouped,
            x = ~year, 
            y = ~n,
            type = 'scatter',
            mode= 'lines',
            color=~PRI_FDA.Industry.Name, colors="Dark2") %>% layout(title = list(text='Adverse Event Causing Industries Over Time'),
                                                                     yaxis = list(type = "log",title="Count"),
                                                                     xaxis = list(title='Year'),legend = list(font = list(size = 10)))
  })
  
  output$click <- renderPrint({
    if (is.null(event_data("plotly_relayout"))) {
      "Zoom and Pan in the line chart also updates the slider"
    } else {
      invisible(updateSliderInput(inputId="year",
                                  value=c(event_data("plotly_relayout")$`xaxis.range[0]`,
                                          event_data("plotly_relayout")$`xaxis.range[1]`)))
    }
  });
  
  output$plot3 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender %in% c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group %in% input$age_grp)
    
    plot3_data=filtered_data %>% filter(age_group!="Age Unknown") %>%
      group_by(PRI_FDA.Industry.Name,age_group) %>%
      summarise(n = n())
    
    plot3_data$age_group=as.factor(plot3_data$age_group)
    
    plot3_data$age_group <- factor(plot3_data$age_group, levels = c("Child (0-14 yrs)", "Youth (15-24 yrs)", "Adult (25-64 yrs)", "Senior (65+ yrs)"))
    mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(14)
    fig=ggplot(plot3_data, aes(x=n, y=PRI_FDA.Industry.Name,fill=PRI_FDA.Industry.Name)) + 
      geom_bar(stat='identity')  +  scale_fill_manual(values=mycolors)+scale_x_log10() + 
      facet_wrap(~age_group, scales = "free_x") + 
      theme(plot.title = element_text(hjust = 0.2),legend.position = "none",axis.title.y=element_blank(),panel.spacing.y = unit(4, "mm")) +
      xlab("Count") + 
      ggtitle("Adverse Event Causing Industries by Age Group")
    
    ggplotly(fig)
    
  });
  
}


shinyApp(ui, server, options = list(height=600))

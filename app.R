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
  titlePanel("Visualization of FDA Reports on Adverse Events"),
  
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
      ),
      selectInput(inputId ='industry',
                  label = h3('Product Industry*: '),
                  choices = c("Baby Food Prod" ,"Bakery Prod/Dough/Mix/Icing","Cereal Prep/Breakfast Food",
                              "Cosmetics","Dietary Conv Food/Meal Replacements","Fishery/Seafood Prod",
                              "Fruit/Fruit Prod","Milk/Butter/Dried Milk Prod" ,
                              "Mult Food Dinner/Grav/Sauce/Special","Nuts/Edible Seed",
                              "Other","Soft Drink/Water","Vegetables/Vegetable Products","Vit/Min/Prot/Unconv Diet(Human/Animal)"),
                  selected = c("Baby Food Prod" ,"Bakery Prod/Dough/Mix/Icing","Cereal Prep/Breakfast Food",
                               "Cosmetics","Dietary Conv Food/Meal Replacements","Fishery/Seafood Prod",
                               "Fruit/Fruit Prod","Milk/Butter/Dried Milk Prod" ,
                               "Mult Food Dinner/Grav/Sauce/Special","Nuts/Edible Seed",
                               "Other","Soft Drink/Water","Vegetables/Vegetable Products","Vit/Min/Prot/Unconv Diet(Human/Animal)"),
                  multiple = TRUE),
      tags$div(tags$h5("*Remove product industry with backspace, add product industry with dropdown selelction"))
                  
    ),
    
    # Show plots
    mainPanel(
      plotlyOutput("plot1"),
      br(), br(),
      plotlyOutput("plot2"),
      verbatimTextOutput("click"),
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
      filter(age_group%in%input$age_grp) %>% 
      filter(PRI_FDA.Industry.Name %in% input$industry)
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
             yaxis = list(title="Count",fixedrange = TRUE),
             xaxis=list(title="", categoryorder = "array", categoryarray = ~PRI_Reported.Brand.Product.Name,
                        ticks = 'outside',
                        tickfont = list(size = 10),
                        tickangle = 15, size = 5,
                        tickprefix=" ",
                        fixedrange = TRUE))  %>% layout(autosize = T, width = 850, height = 400)
  })
  
  output$plot2 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender %in% c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group%in%input$age_grp) %>% 
      filter(PRI_FDA.Industry.Name %in% input$industry)
    
    industry_year_grouped=filtered_data %>%
      group_by(PRI_FDA.Industry.Name,year) %>%
      summarise(n = n())
    mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(14)
    
    p<-ggplot(industry_year_grouped, aes(x=year, y=n,group=PRI_FDA.Industry.Name)) +
      geom_line(aes(color=PRI_FDA.Industry.Name))+ scale_color_manual(values=mycolors) +scale_y_log10() +
      ggtitle('Adverse Event Causing Industries Over Time') +xlab("Year") +ylab("Count") +
      theme(legend.title=element_blank(),legend.text=element_text(size=9))
    
    ggplotly(p) %>% layout(yaxis = list(fixedrange = TRUE))%>% layout(autosize = T, width = 850, height = 400)
})
    output$click <- renderPrint({
        invisible(updateSliderInput(inputId="year",
                                    value=c(event_data("plotly_relayout")$`xaxis.range[0]`,
                                            event_data("plotly_relayout")$`xaxis.range[1]`)))
      
    });

  
  output$plot3 <- renderPlotly({
    filtered_data <-data %>% 
      {if (input$gender!="All") filter(.,CI_Gender==input$gender) else (filter(.,CI_Gender %in% c("Male","Female","Not Available")))} %>%
      filter(year>=input$year[1]) %>%
      filter(year<=input$year[2]) %>%
      filter(age_group %in% input$age_grp) %>% 
      filter(PRI_FDA.Industry.Name %in% input$industry)
    
    plot3_data=filtered_data %>% filter(age_group!="Age Unknown") %>%
      group_by(PRI_FDA.Industry.Name,age_group) %>%
      summarise(n = n())
    
    plot3_data$age_group=as.factor(plot3_data$age_group)
    
    plot3_data$age_group <- factor(plot3_data$age_group, levels = c("Child (0-14 yrs)", "Youth (15-24 yrs)", "Adult (25-64 yrs)", "Senior (65+ yrs)"))
    mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(14)
    fig=ggplot(plot3_data, aes(x=n, y=PRI_FDA.Industry.Name,fill=PRI_FDA.Industry.Name)) + 
      geom_bar(stat='identity')  +  scale_fill_manual(values=mycolors)+scale_x_log10() + 
      facet_wrap(~age_group, scales = "free_x") + 
      theme(plot.title = element_text(hjust = -4),legend.position = "none",axis.title.y=element_blank(),panel.spacing.y = unit(10, "mm")) +
      xlab("Count") + 
      ggtitle("Adverse Event Causing Industries by Age Group")
    
    ggplotly(fig) %>% layout(xaxis = list(fixedrange = TRUE)) %>% layout(autosize = F, width = 700, height = 500)
    
  });
  
}


shinyApp(ui, server, options = list(height=600))

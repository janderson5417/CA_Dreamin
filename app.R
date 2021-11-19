library(shinydashboard)
library(DT)
require(RcppRoll)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rsconnect)

#load data
temp = read.csv('Weather20-cat.csv')
old = read.csv('1951.csv')

#prepare data
temp30 = temp %>% 
  mutate(diffT = Temp_max - Temp_min) %>%
  unite(Date_Time, Date, Time, sep = " ", remove = TRUE) %>%
  mutate(Date_Time = ymd_hm(Date_Time), .keep = 'unused',
         Month = month(Date_Time),
         Year = year(Date_Time),
         Date = date(Date_Time),
         Year = as.integer(Year))

fifties = old %>%
  mutate(diffT = Temp_max - Temp_min) %>%
  unite(Date_Time, Date, Time, sep = " ", remove = TRUE) %>%
  mutate(Date_Time = ymd_hm(Date_Time), .keep = 'unused',
         Month = month(Date_Time),
         Year = year(Date_Time),
         Date = date(Date_Time),
         Year = as.integer(Year))

##ui.R##
ui = dashboardPage(
  dashboardHeader(
    title = 'Climate variations in selected California cities',
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 200,
    selectizeInput('city', label = h4('Select a weather station to look in more detail'),
                choices = sort(unique(temp30$Station)), selected = 'BAKRSFLD.A'),
    
    selectizeInput('year', label = h4('Select a year to compare to early data'),
                   choices = sort(unique(temp30$Year)), selected = '2020')
  ),
  
  dashboardBody(
  
    fluidRow(
      box(title = 'Number of days >100 degF',
             plotOutput('hotplot', height = 225)
             ),
      box(title = 'Annual Precipitation, inches',
              plotOutput('rainplot', height = 225)
              )
      ),
    
    fluidRow(
      box(title = '45d Rolling Average, Daily Max Temperature (degF)',
             plotOutput('roll', height = 300)),

      box(height = 150, title = 'Benchmark Comparison',
          tableOutput('comp')),
      box(height = 150,title = 'How does the older temperature data compare to later years?
      Select a year, and let\'s examine the p-values.',
          tableOutput('pvals')),
      ),
    fluidRow(
      box(height = 250, background = 'blue',
          tags$div(
            tags$p('What\'s \"normal\" for a dynamic system? Climate researchers use
          periods of 30 years to benchmark temperature trends. Because they are
          updated once per decade, \"normal\" gradually changes. 1951 is commonly
          referenced for comparison, as NASA\'s Goddard Institute for Space Studies 
          began analysing temperature data in 1980.'),
          tags$p('You might notice minor abnormalities within some of the data. There are very few
                 active weather stations that have complete, good quality data! '),
          tags$a(href = 'https:\\https://earthobservatory.nasa.gov/world-of-change/global-temperatures', 
          'See here for more details.'))),
      box(height = 250, background = 'blue',
          tags$div(
            tags$p('We can use a probability test to determine if temperature observations from
          two different years are stastically the same or not. The p-value, commonly set at 0.05 (or, 5%), 
          serves as a threshold for making that determination. Because the data suggest that the variance, or
                   deviation away from the mean is changing annually, we rely on the Welch\'s Test.'),
            tags$p('In the table above, if the resulting p-value is equal to or less than
          0.05, we can say we are 95% confident there is an actual difference in the observations
          not due to random chance.'),
            tags$p('Note--weather stations that do not have this early data set result in no p-values
                   computed.'))
          )
      )
    )
)
server = function(input, output) {
  
  output$hotplot = renderPlot({
  
  hotdays = temp30 %>%
    filter(Station == input$city) %>%
    group_by(Year) %>%
    mutate(hot = ifelse(Temp_max >= 100, 1, 0)) %>%
    summarise(sum_hot = sum(hot))
  
  ggplot(hotdays, aes(x = Year, y = sum_hot)) + geom_col(fill = '#FF0000') +
       xlab('Year') + ylab('Number of days')

   })
  
   output$rainplot = renderPlot({
     rain = temp30 %>%
       filter(Station == input$city) %>%
       group_by(Year) %>%
       summarise(annual_rain = sum(Precip))
     
     ggplot(rain, aes(x = Year, y = annual_rain)) + geom_col(fill = '#0080FF') +
        xlab('Year') + ylab('Rainfall Amount (in)')
     })

  output$roll = renderPlot({
    rolling = temp30 %>%
      filter(Station == input$city) %>%
      mutate(rollmax = roll_max(Temp_max, n = 45, align = 'right', fill = NA))
    
    ggplot(rolling, aes(Date_Time, rollmax)) + geom_line(color = '#FF0000') +
      geom_smooth()
  })
  
  output$comp = renderTable({
    
    fifties_by_station = fifties %>%
      filter(Station == input$city) %>%
      group_by(Year) %>%
      mutate(hot = ifelse(Temp_max >= 100, 1, 0)) %>%
      summarise(Sum.Hot.Days = sum(hot),
                Avg.T.Range = round(mean(diffT),3),
                Max.T = max(Temp_max),
                Min.T = min(Temp_min),
                Ann.Rain = sum(Precip)) %>%
      slice(1)
    
    summary_temp30 = temp30 %>%
      filter(Station == input$city & Year == input$year) %>%
      group_by(Year) %>%
      mutate(hot = ifelse(Temp_max >= 100, 1, 0)) %>%
      summarise(Sum.Hot.Days = sum(hot),
                Avg.T.Range = round(mean(diffT),3),
                Max.T = max(Temp_max),
                Min.T = min(Temp_min),
                Ann.Rain = sum(Precip))
    
    table = bind_rows(fifties_by_station, summary_temp30, id = NULL)
    table
    
  })
  output$pvals = renderTable({
    one_year = temp30 %>%
      filter(Station == input$city & Year == input$year)
    
    fifties2 = fifties %>%
      filter(Station == input$city)
    
    pval_rng = t.test(one_year$diffT, fifties$diffT)
    pval_maxT = t.test(one_year$Temp_max, fifties$Temp_max)
    pval_minT = t.test(one_year$Temp_min, fifties$Temp_min)
  
  
    pvals = data.frame('Year' = as.integer(input$year), 'Avg.T.Range_pval' = 
                         round(pval_rng$p.value,5), 'Max.T_pval' = 
                         (pval_maxT$p.value), 
                       'Min.T_pval' = (pval_minT$p.value))
    
    pvals

  }, digits = 5)

    
}

shinyApp(ui = ui, server = server)

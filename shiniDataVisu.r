library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(wordcloud)
cap = read.csv('./Data/25_Complaints_against_police.csv')
#hr = read.csv('./Data/35_Human_rights_violation_by_police.csv')
names(cap)[1] = 'state'
c = cap %>% group_by(state,Year) %>% 
  summarise(alleged = sum(CPA_._Complaints_Received.Alleged), registered = sum(CPA_._Cases_Registered)) %>% 
  arrange(-alleged) %>%
  mutate(total = alleged + registered) %>% arrange(-total)


ui = fluidPage(
  headerPanel('Cases against police'),
  sliderInput(inputId = 'Y1', label = 'Pick a year',min = 2001, max = 2010, step = 1,value = 2001),
  plotOutput(outputId = 'point'),
  headerPanel('Wordcloud of States wrt complaints'),
  sidebarPanel(
    selectInput(inputId = 'c2',label = 'Complaint type',choices = c('alleged','registered','total'),
                selected = 'total'),
    selectInput(inputId = 'Y2', label = 'Pick a Year', choices = 2001:2010, selected = 2001)
  ),
  mainPanel(plotOutput(outputId = 'wc'))
  
)

server = function(input, output){
  output$point = renderPlot(
    c %>% filter(Year == input$Y1) %>% 
      ggplot(aes(x = alleged, y = registered,col = state, size = 100*total)) + geom_point() +
      theme(legend.position = 'none') + scale_size_continuous(range = c(3, 15))
  )
  output$wc = renderPlot(
    wordcloud(c$state, c$total, min.freq = 0, colors=brewer.pal(8, "Dark2"))
  )
}

shinyApp(ui, server)
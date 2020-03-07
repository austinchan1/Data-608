library(ggplot2)
library(dplyr)
library(shiny)

#read data in
data_path = "./Data/cleaned-cdc-mortality-1999-2010-2.csv"

raw = read.csv(data_path, stringsAsFactors = F)

#create national crude death rates
national = raw %>%
  dplyr::group_by(ICD.Chapter,Year) %>%
  summarise(Deaths = sum(Deaths), Population = sum(Population)) %>%
  mutate(State = "United States", Crude.Rate = round(100000*(Deaths/Population),digits = 1)) %>%
  select(ICD.Chapter, State, Year, Deaths, Population, Crude.Rate)

#bind national death rates with state death rates
df = bind_rows(national,raw)


#create ui
ui <- fluidPage(
  headerPanel('Crude Death Rate Over Time'), #title
  sidebarPanel(
    selectInput('death', 'Cause of Death', sort(unique(df$ICD.Chapter)), selected='Neoplasms'), #add cause of death as an input
    selectInput('state', 'State', sort(unique(raw$State)), selected = 'NY') #add state as an input
  ),
  mainPanel(
    plotOutput('plot1', height = 500, width = 800) #show plot at specific size
  )
)

#create server
server <- function(input, output) {
  
  #plot output
  output$plot1 <- renderPlot({
    
    national_subset = subset(national,ICD.Chapter == input$death) #filter national death rate by cause of death
    raw_subset = subset(raw,ICD.Chapter == input$death & State == input$state) #filter state death rates by cause of death and state
    subset_grouped = bind_rows(national_subset,raw_subset) #put together the two dataframes
    title = paste(input$death,"Deaths per 100,000 People in", input$state, sep = " ") #create title
    
    #plot death rate by state over time
    ggplot(data = subset_grouped, aes(x = Year, y = Crude.Rate, group = factor(State, levels = c(input$state,"United States")))) +
      geom_line(aes(color = factor(State, levels = c(input$state,"United States")))) +
      geom_point(aes(color = factor(State, levels = c(input$state,"United States")))) +
      ggtitle(title) +
      labs(color = "State", x = "Year", y = "Deaths per 100,000 People")
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)


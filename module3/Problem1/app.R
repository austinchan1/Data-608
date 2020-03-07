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

#combine national and state death rates and only fetch 2010 data
df = bind_rows(national,raw)
df = subset(df,Year == 2010)
national = subset(national, Year == 2010)

#create ui
ui <- fluidPage(
    headerPanel('Crude Death Rate 2010'), #add title
    sidebarPanel(
        selectInput('death', 'Cause of Death', sort(unique(df$ICD.Chapter)), selected='Neoplasms') #add cause of death as an input
    ),
    mainPanel(
        plotOutput('plot1', height = 1000, width = 800) #show plot at a specific size
    )
)

#create server
server <- function(input, output) {
    
    #plot output
    output$plot1 <- renderPlot({
        
        subsetdf = subset(df,ICD.Chapter == input$death & Year == 2010) #subset data by cause of death
        subsetdf$State = factor(subsetdf$State, levels = subsetdf$State[order(subsetdf$Crude.Rate)]) #order states by highest death rate to lowest
        title = paste(input$death,"Deaths per 100,000 People", sep = " ") #create title
        
        #plot death rates by state in descending order of death rates
        ggplot(data=subsetdf, aes(x=State, y=Crude.Rate)) +
            geom_bar(stat="identity", fill="steelblue")+
            geom_text(aes(label=Crude.Rate),hjust = 1.2, color="white", size=4.5)+
            theme_minimal() +
            coord_flip() +
            ggtitle(title) +
            labs(y = "Deaths per 100,000 People", x = "State")
        
        
    })
    
    
}

shinyApp(ui = ui, server = server)



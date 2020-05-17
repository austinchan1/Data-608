

library(readxl)
library(dplyr)
library(sqldf)


library(shiny)
library(leaflet)
library(htmltools)

library(ggplot2)


categorical_dict = read_xlsx("../FinalProjectTest/ScorecardDataframe.xlsx", sheet = "Sheet1")

categorical_dict$Value = as.character(categorical_dict$Value)

raw_data = read.csv("../FinalProjectTest/Most-Recent-Cohorts-All-Data-Elements.csv", stringsAsFactors = F)
raw_data$LATITUDE = as.numeric(raw_data$LATITUDE)
raw_data$LONGITUDE = as.numeric(raw_data$LONGITUDE)

cols_to_join = raw_data[,names(raw_data) %in% unique(categorical_dict$ColName)]


col_joiner = function(column,col_name){
    
    reference_df = subset(categorical_dict, ColName == col_name)
    names(reference_df) = c("ColName",col_name,"Label")
    
    df_to_join = data.frame(rename_column = as.character(column))
    names(df_to_join) = col_name
    
    joined = left_join(df_to_join,reference_df,by = col_name)
    
    output = joined$Label
    
    return(output)
    
}



testo = lapply(names(cols_to_join), function(x){
    
    return(col_joiner(cols_to_join[,x],x))
    
})


final_df = data.frame(t(as.data.frame(do.call(rbind,testo))))
rownames(final_df) = NULL
names(final_df) = names(cols_to_join)

numerical_data = select(raw_data,
                        "INSTNM", #institution name
                        "CITY",
                        "STABBR",
                        "ZIP",
                        "INSTURL", #school website
                        "NPCURL", #school tuition estimator
                        "LATITUDE",
                        "LONGITUDE",
                        "TUITIONFEE_IN",
                        "TUITIONFEE_OUT",
                        "SATVRMID",
                        "SATMTMID",
                        "ACTCMMID"
)


trimmed_data = cbind(numerical_data,final_df)

trimmed_data$TUITIONFEE_IN = as.numeric(trimmed_data$TUITIONFEE_IN)
trimmed_data$TUITIONFEE_IN[is.na(trimmed_data$TUITIONFEE_IN)] = -1

trimmed_data$TUITIONFEE_OUT = as.numeric(trimmed_data$TUITIONFEE_OUT)
trimmed_data$TUITIONFEE_OUT[is.na(trimmed_data$TUITIONFEE_OUT)] = -1

trimmed_data$SATVRMID = as.numeric(trimmed_data$SATVRMID)
trimmed_data$SATVRMID[is.na(trimmed_data$SATVRMID)] = -1

trimmed_data$SATMTMID = as.numeric(trimmed_data$SATMTMID)
trimmed_data$SATMTMID[is.na(trimmed_data$SATMTMID)] = -1

trimmed_data$ACTCMMID = as.numeric(trimmed_data$ACTCMMID)
trimmed_data$ACTCMMID[is.na(trimmed_data$ACTCMMID)] = -1


get_weight = function(column,item_selection,radio_selection){
    
    if(radio_selection == "Very Important" && typeof(column) == "character"){
        
        score = (column %in% item_selection) * 100
        
    }
    
    else if(radio_selection == "Very Important" && typeof(column) != "character"){
        
        score = between(column,item_selection[1],item_selection[2]) * 100
        
    }
    
    else if(radio_selection == "Somewhat Important" && typeof(column) == "character"){
        
        score = (column %in% item_selection) * 25
        
    }
    
    else if(radio_selection == "Somewhat Important" && typeof(column) != "character"){
        
        score = between(column,item_selection[1],item_selection[2]) * 25
        
    }
    
    else if(radio_selection == "Not Important"){
        
        score = 0
        
    }
    
    
    return(score)
    
} 



df = trimmed_data

ui <- fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    headerPanel('College Explorer'),
    sidebarPanel(
        selectInput('degree_type','Degree Type', sort(unique(df$PREDDEG)), selected ="Predominantly bachelor's-degree granting"),
        selectInput('state', 'Select a State (or multiple States)', sort(unique(df$ST_FIPS)), selected=c("New York","Pennsylvania","Vermont","New Hampshire"), multiple = T),
        radioButtons('state_radio', 'State Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Not Important"),
        sliderInput('tuition_in','Yearly In-State Tuition Range', min = 0, max = 75000, value = c(0,7500)),
        radioButtons('tuition_in_radio', 'In-State Tuition Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Very Important"),
        sliderInput('sat_math','Median SAT Math Score Range', min = 0, max = 800, value = c(650,800)),
        radioButtons('sat_math_radio', 'SAT Math Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Very Important"),
        sliderInput('sat_reading','Median SAT Reading Score Range', min = 0, max = 800, value = c(0,600)),
        radioButtons('sat_reading_radio', 'SAT Reading Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Not Important"),
        sliderInput('act','Median ACT Cumulative Score Range', min = 0, max = 35, value = c(25,35)),
        radioButtons('act_radio', 'ACT Cumulative Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Very Important"),
        selectInput('size_set','Size and Setting', sort(unique(df$CCSIZSET)), selected = "Four-year, large, primarily nonresidential", multiple = T),
        radioButtons('size_set_radio', 'Size and Setting Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Not Important"),
        selectInput('control','Public Institution or Private Institution?', sort(unique(df$CONTROL)), selected = "Public", multiple = T),
        radioButtons('control_radio', 'Public or Private Importance', c("Very Important","Somewhat Important","Not Important"), selected = "Somewhat Important")
        
        
    ),
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Welcome Page", 
                             h2("Welcome!"),
                             h4("Please start by filling out the short survey located on the sidebar to the right."),
                             br(),
                             h3("What is the College Explorer?"),
                             "The College Explorer is a web tool created in R Shiny to help discover and explore the higher education landscape in the United States.",
                             br(),
                             h3("How does the College Explorer work?"),
                             "The College explorer uses a series of user inputs to filter down a list of all higher education institutions in the United States into a list of 20 schools. The schools are ranked by order of compatability based on your survey responses. Afterwards, maps and charts are dynamically generated to display and compare your top 20 schools."
                             
                             
                    ),
                    tabPanel("School Map",
                             h2("School Map"),
                             "Click on schools for additional information. (-1 indicates missing values)",
                             br(),
                             leafletOutput('map')
                    ),
                    tabPanel("Tuition Comparison",
                             h2("In-State Tuition (per year)"),
                             "(-1 indicates missing values)",
                             plotOutput("tuition_graph_in"),
                             br(),
                             h2("Out-State Tuition (per year)"),
                             "(-1 indicates missing values)",
                             plotOutput("tuition_graph_out")
                    )
        )
        
    )
)


server <- function(input, output) {
    
    
    
    output$map <- renderLeaflet({
        
        subsetdf = subset(df,PREDDEG %in% input$degree_type)
        
        subsetdf$basic_score = get_weight(subsetdf$ST_FIPS, input$state, input$state_radio) +
            get_weight(subsetdf$CCSIZSET, input$size_set, input$size_set_radio) +
            get_weight(subsetdf$CONTROL, input$control, input$control_radio) +
            get_weight(subsetdf$SATMTMID, input$sat_math, input$sat_math_radio) +
            get_weight(subsetdf$SATVRMID, input$sat_reading, input$sat_reading_radio) +
            get_weight(subsetdf$ACTCMMID, input$act, input$act_radio) +
            get_weight(subsetdf$TUITIONFEE_IN, input$tuition_in, input$tuition_in_radio)
        
        subsetdf$score = subsetdf$basic_score/max(subsetdf$basic_score)
        
        
        subsetdf$marker_labels = paste(subsetdf$INSTNM,", ",round(subsetdf$score,3) * 100,'% Match', sep = "")
        
        sorted = subsetdf[order(-subsetdf$score),]
        topx = sorted[1:20,]
        
        pal = colorNumeric("RdYlGn",subsetdf$score)
        
        mini_labels = lapply(seq(nrow(topx)), function(x){
            
            paste0(sep="",
                   "<b>",
                   topx[x,"INSTNM"],
                   "</a></b><br/>",
                   topx[x,"CITY"],
                   ", ",
                   topx[x,"STABBR"],
                   " ",
                   topx[x,"ZIP"]
            )
            
        })
        
        map_labels = lapply(seq(nrow(topx)), function(x){
            
            paste0(sep="",
                   "<b>",
                   topx[x,"INSTNM"],
                   "</b><br/>",
                   topx[x,"CITY"],
                   ", ",
                   topx[x,"STABBR"],
                   " ",
                   topx[x,"ZIP"],
                   "<br/>In-State Tuition: $",
                   topx[x,"TUITIONFEE_IN"],
                   "<br/>SAT Math: ",
                   topx[x,"SATMTMID"],
                   "<br/>SAT Reading: ",
                   topx[x,"SATVRMID"],
                   "<br/>ACT Cumulative: ",
                   topx[x,"ACTCMMID"],
                   "<br/>Size and Setting: ",
                   topx[x,"CCSIZSET"],
                   "<br/>Public or Private: ",
                   topx[x,"CONTROL"]
            )
            
        })
        
        leaflet(topx) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addCircleMarkers(lat = ~LATITUDE, 
                             lng = ~LONGITUDE, 
                             label = lapply(mini_labels, htmltools::HTML),
                             popup = lapply(map_labels, htmltools::HTML),
                             color = "white",
                             fillColor = ~pal(score),
                             fillOpacity = 0.9,
                             radius = ~score * 8,
                             stroke = F
            ) %>%
            addLegend("bottomright", 
                      pal = pal, 
                      values = subsetdf$score,
                      title = "Fit Score (1 = Perfect Fit)",
                      opacity = 1)
        
        
        
    })
    
    
    output$tuition_graph_in <- renderPlot({
        
        subsetdf = subset(df,PREDDEG %in% input$degree_type)
        
        subsetdf$basic_score = get_weight(subsetdf$ST_FIPS, input$state, input$state_radio) +
            get_weight(subsetdf$CCSIZSET, input$size_set, input$size_set_radio) +
            get_weight(subsetdf$CONTROL, input$control, input$control_radio) +
            get_weight(subsetdf$SATMTMID, input$sat_math, input$sat_math_radio) +
            get_weight(subsetdf$SATVRMID, input$sat_reading, input$sat_reading_radio) +
            get_weight(subsetdf$ACTCMMID, input$act, input$act_radio) +
            get_weight(subsetdf$TUITIONFEE_IN, input$tuition_in, input$tuition_in_radio)
        
        subsetdf$score = subsetdf$basic_score/max(subsetdf$basic_score)
        
        
        subsetdf$marker_labels = paste(subsetdf$INSTNM,", ",round(subsetdf$score,3) * 100,'% Match', sep = "")
        
        sorted = subsetdf[order(-subsetdf$score),]
        topx = sorted[1:20,]
        
        ggplot(topx, aes(x = reorder(INSTNM,-TUITIONFEE_IN), y = TUITIONFEE_IN)) + #sorts state counts in descending order
            geom_bar(stat = "identity",aes(fill = TUITIONFEE_IN), colour = "black") + #creates bar graph colored by number of companies
            coord_flip() + #flips x and y coordinates to make graph longer
            geom_text(stat = 'identity', aes(label = paste('$',TUITIONFEE_IN, sep = "")), hjust = -0.2) + #adds number label to each bar 
            #ggtitle("In-State Tuition Comparison") + #adds title
            guides(fill = F) + #removes legend
            theme_minimal() + #minimal theme for less intrusive visual elements
            theme(axis.title.x = element_blank(), #remove x-axis label
                  axis.text.x = element_blank(), #remove x-axis text
                  axis.ticks.x = element_blank(), #remove x-axis tick marks
                  axis.title.y = element_blank()) #remove y-axis label
        
    })
    
    
    output$tuition_graph_out <- renderPlot({
        
        subsetdf = subset(df,PREDDEG %in% input$degree_type)
        
        subsetdf$basic_score = get_weight(subsetdf$ST_FIPS, input$state, input$state_radio) +
            get_weight(subsetdf$CCSIZSET, input$size_set, input$size_set_radio) +
            get_weight(subsetdf$CONTROL, input$control, input$control_radio) +
            get_weight(subsetdf$SATMTMID, input$sat_math, input$sat_math_radio) +
            get_weight(subsetdf$SATVRMID, input$sat_reading, input$sat_reading_radio) +
            get_weight(subsetdf$ACTCMMID, input$act, input$act_radio) +
            get_weight(subsetdf$TUITIONFEE_IN, input$tuition_in, input$tuition_in_radio)
        
        subsetdf$score = subsetdf$basic_score/max(subsetdf$basic_score)
        
        
        subsetdf$marker_labels = paste(subsetdf$INSTNM,", ",round(subsetdf$score,3) * 100,'% Match', sep = "")
        
        sorted = subsetdf[order(-subsetdf$score),]
        topx = sorted[1:20,]
        
        ggplot(topx, aes(x = reorder(INSTNM,-TUITIONFEE_OUT), y = TUITIONFEE_OUT)) + #sorts state counts in descending order
            geom_bar(stat = "identity",aes(fill = TUITIONFEE_OUT), colour = "black") + #creates bar graph colored by number of companies
            coord_flip() + #flips x and y coordinates to make graph longer
            geom_text(stat = 'identity', aes(label = paste("$",TUITIONFEE_OUT, sep = "")), hjust = -0.2) + #adds number label to each bar 
            #ggtitle("In-State Tuition Comparison") + #adds title
            guides(fill = F) + #removes legend
            theme_minimal() + #minimal theme for less intrusive visual elements
            theme(axis.title.x = element_blank(), #remove x-axis label
                  axis.text.x = element_blank(), #remove x-axis text
                  axis.ticks.x = element_blank(), #remove x-axis tick marks
                  axis.title.y = element_blank()) #remove y-axis label
        
    })
    
    
}

shinyApp(ui = ui, server = server)





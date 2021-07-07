#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(highcharter)
library(plotly)


df <- read.csv(
  file = "C:\\Users\\payto\\OneDrive\\Documents\\Data Project\\forbes_billionaires.csv",
  header = TRUE
)

head(df, n = 15)
sum(is.na(df$Self_made)) # 18
nrow(df)
str(df)
length(df$Education)

mode(df$Source)

uanique(df$Status)
unique(df$Citizenship)

str(df$Education)




## Education ##

splits <- unlist(str_split(df$Education, ", ", n = 2))
head(splits, n = 30)
unique(splits)

df2 <- data.frame(
  col1 = splits
)
df2

df_string <- df2[!grepl("Univer|Polytech|School|College|Academ|Instit|State|Province|SUNY|di|de
                        |Los|UCLA|Paris|Oxford|Wuhan|San|Des|Bejing|Funda|Faculd|KAIST|Middle|U of", df2$col1),]
head(df_string, n = 15)
unique(df_string)
length(df_string)

## Relationship Status ##

status <- df %>%
  group_by(Status) %>%
    summarise(
      count = n(),
      med_worth = median(NetWorth, na.rm = TRUE)
)

head(status)

status[status == ''] <- "Not Defined"
status

rate <- status$med_worth / status$count
per_100 <- rate * 100
per_100
status['worth_per_100'] <- per_100
status


## Country ##

country <- df %>%
  group_by(Country) %>%
    summarise(
      count = n()
)
country <- country[order(-country$count),]
head(country, n = 15)

mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/custom/world-highres.js"))

glimpse(mapdata)

world <- mapdata %>%
  select(name) %>%
  arrange(name)
world

names(world) <- c("Country")
world


country[country == 'United States'] <- 'United States of America'
country

hcmap(
  "https://code.highcharts.com/mapdata/custom/world-highres.js", 
  data = country, 
  value = "count",
  joinBy = c("name", "Country"),
  name = "Count"
) %>%
  hc_mapNavigation(enabled = TRUE)

#world_country <- left_join(country, world)


### Extra Variables ###

wealth <- sum(df$NetWorth)
wealth <- as.character(wealth)
char_wealth <- substr(wealth, start = 1, stop = 2)


self_made <- (sum(df$Self_made, na.rm = TRUE) / (nrow(df) - sum(is.na(df$Self_made))))
self_made



## UI ##

head = dashboardHeader(
  title = "Billionaires Data Analysis",
  titleWidth = 400
)

body <- dashboardBody(
  
  fluidRow(
    
    valueBoxOutput("total_wealth"),
    valueBoxOutput("avg_kids"),
    valueBoxOutput("self_made")
    
  ),
  
  fluidRow(
    
    box(
      title = "Histogram of Ages",
      solidHeader = TRUE,
      plotlyOutput("hist_plot")
    ),
    
    box(
      highchartOutput("hc1"),
      solidHeader = TRUE
    )
  ),
  
  fluidRow(
    
    box(
      highchartOutput("hcmap1"),
      width = 12,
      solidHeader = TRUE
    )
  )
)

ui <- dashboardPage(
  head,
  dashboardSidebar(disable = TRUE),
  body
)


## Server ##

server <- function(input, output) {
  
  output$total_wealth <- renderValueBox(

    valueBox(
      paste0("$", char_wealth, " Trillion"), subtitle = "Total Wealth", icon = icon("money-bill"),
      color = "green"
    )
  )
  
  output$avg_kids <- renderValueBox(
    valueBox(
      paste0(round(mean(df$Children, na.rm = TRUE))), subtitle = "Average # of Kids",
      icon = icon("child"), color = "light-blue"
    )
  )
  
  output$self_made <- renderValueBox(
    valueBox(
      paste0(
        round(sum(df$Self_made, na.rm = TRUE) / (nrow(df) - sum(is.na(df$Self_made))) * 100, 1), "%"
      ),
      subtitle = "Self Made", color = "purple", icon = icon("user-tie")
    )
  )
  
  output$hist_plot <- renderPlotly(
    
    ggplotly(ggplot(df, aes(x = Age)) +
      geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
      geom_density(alpha = .2, fill = "#FF6666", size = 0.75) +
      theme_minimal()
    )
    
  )
  
  output$hc1 <- renderHighchart(
    
    highchart() %>%
      hc_yAxis_multiples(
        list(lineWidth = 3, lineColor = 'blue', title = list(text = "Net Worth")),
        list(lineWidth = 3, lineColor = 'red', title = list(text = "Net Worth per 100 People"))
      ) %>%
      hc_add_series(data = status$med_worth, color = 'blue', type = 'column') %>%
      hc_add_series(data = status$worth_per_100, color = 'red', type = 'column') %>%
      hc_xAxis(categories = status$Status, title = list(text = "Status")) %>%
      hc_title(
        text = "Original and Scaled Net Worth by Relationship Status <b>(In Billions $)</b>",
        margin = 10
      )
  )
  
  output$hcmap1 <- renderHighchart(
    
    hcmap(
      "https://code.highcharts.com/mapdata/custom/world-highres.js", 
      data = country, 
      value = "count",
      joinBy = c("name", "Country"),
      name = "Count"
    ) %>%
      hc_mapNavigation(enabled = TRUE)
  )
}

shinyApp(ui, server)



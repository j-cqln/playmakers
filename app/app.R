library(shiny)
library(leaflet)

library(dplyr)
library(DT)
library(readr)

FIRST_YEAR <- 2021
LAST_YEAR <- 2023

ranks <- read.csv("data/ranks.csv")

server <- function(input, output, session) {
  data <- reactive({
    ranks %>% filter(season == input$year)
  })
  
  output$box <- renderUI({
    img(src = paste0(input$year, "_box.png"), height = "400px")
  })
  
  output$quality <- renderUI({
    img(src = paste0(input$year, "_quality.png"), height = "400px")
  })
  
  output$forwards <- renderDataTable({
    r <- data() %>%
      filter(position_group == "F") %>%
      arrange(playmaking_rank) %>%
      rename(player = shooter,
             avg_xRebounds = avg_xrebounds,
             avg_xRV = avg_rebound_value) %>%
      select(player, goals, assists, avg_xRebounds, avg_xRV, assists_rank, playmaking_rank, rank_change)
    
    datatable(r, options = list(scrollX = TRUE,
                                scrollY = "250px")) %>%
      formatRound("avg_xRebounds", digits = 3) %>%
      formatRound("avg_xRV", digits = 3)
  })
  
  output$defencemen <- renderDataTable({
    r <- data() %>%
      filter(position_group == "D") %>%
      arrange(playmaking_rank) %>%
      rename(player = shooter,
             avg_xRebounds = avg_xrebounds,
             avg_xRV = avg_rebound_value) %>%
      select(player, goals, assists, avg_xRebounds, avg_xRV, assists_rank, playmaking_rank, rank_change)
    
    datatable(r, options = list(scrollX = TRUE,
                                scrollY = "250px")) %>%
      formatRound("avg_xRebounds", digits = 3) %>%
      formatRound("avg_xRV", digits = 3)
  })
  
  observe({
    r <- data()
  })
}

ui <- fluidPage(
  titlePanel("Playmakers"),
  
  tabsetPanel(
    tabPanel("Ranking",
             fluid = TRUE,
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Rethinking playmakers and player archetypes"),
                 p("Expected goals, rebounds, and rebound value in player evaluation"),
                 p("Data from MoneyPuck.com."),
                 br(),
                 
                 selectInput(inputId = "year",
                             label = "Season",
                             choices = c(FIRST_YEAR:LAST_YEAR),
                             selected = LAST_YEAR)
               ),
               
               mainPanel(
                 fluidRow(
                   column(h4("Forwards"),
                          DT::dataTableOutput("forwards"),
                          width = 12),
                   column(h4("Defencemen"),
                          DT::dataTableOutput("defencemen"),
                          width = 12)
                 )
               )
             )
    ),
    
    tabPanel("Clustering",
             fluid = TRUE,
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Rethinking playmakers and player archetypes"),
                 p("Expected goals, rebounds, and rebound value in player evaluation"),
                 p("Data from MoneyPuck.com."),
                 br(),
                 
                 selectInput(inputId = "year",
                             label = "Season",
                             choices = c(FIRST_YEAR:LAST_YEAR),
                             selected = LAST_YEAR)
               ),
               
               mainPanel(
                 uiOutput("box"),
                 uiOutput("quality")
               )
             )
    )
  )
)

shinyApp(ui = ui, server = server)
FIRST_YEAR <- 2021
LAST_YEAR <- 2023

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
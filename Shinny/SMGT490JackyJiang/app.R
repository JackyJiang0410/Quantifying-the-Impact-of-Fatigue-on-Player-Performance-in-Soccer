library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(DT)

# Load data
data <- read_csv("Fatigue Performance Summary.csv")

ui <- fluidPage(
  titlePanel("Fatigue and Performance in EPL 2015–16"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player ID:", choices = c("All", unique(data$player_id))),
      selectInput("metric", "Performance Metric:", 
                  choices = c("Pass Accuracy", "Duel Win Rate", "Shot Success Rate"),
                  selected = "Pass Accuracy"),
      sliderInput("max_games", "Filter by Total Games Played:", min = 0, max = 38, value = 15)
    ),
    
    mainPanel(
      plotlyOutput("scatterPlot"),
      DTOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- data
    if (input$player != "All") {
      df <- df[df$player_id == as.numeric(input$player), ]
    }
    df <- df[df$total_games_played <= input$max_games, ]
    df
  })
  
  output$scatterPlot <- renderPlotly({
    metric_col <- case_when(
      input$metric == "Pass Accuracy" ~ "pass_accuracy",
      input$metric == "Duel Win Rate" ~ "duel_win_rate",
      input$metric == "Shot Success Rate" ~ "shot_success_rate"
    )
    
    df <- filtered_data()
    
    p <- ggplot(df, aes_string(x = "total_events", y = metric_col, color = "factor(player_id)",
                               text = "paste('Player ID:', player_id, '<br>Match ID:', match_id)")) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal() +
      labs(
        title = paste(input$metric, "vs Match Intensity"),
        x = "Total Events (Match Intensity)",
        y = input$metric,
        color = "Player ID"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summaryTable <- renderDT({
    df <- filtered_data()
    df %>%
      group_by(player_id) %>%
      summarise(
        AvgPassAccuracy = round(mean(pass_accuracy, na.rm = TRUE), 3),
        AvgDuelWinRate = round(mean(duel_win_rate, na.rm = TRUE), 3),
        AvgShotSuccess = round(mean(shot_success_rate, na.rm = TRUE), 3),
        GamesPlayed = max(total_games_played),
        .groups = "drop"
      ) %>%
      datatable(options = list(pageLength = 5))
  })
}

shinyApp(ui, server)


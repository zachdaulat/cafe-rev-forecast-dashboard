library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(prophet)
library(lubridate)

# Load data
cafe_txns <- read_excel("data/cafe_txns.xlsx") |>
  mutate(
    txn_hms = hms::as_hms(transaction_time),
    txn_dt = transaction_date + txn_hms,
    txn_date = date(transaction_date),
    txn_hour = hour(txn_hms),
    txn_wday = wday(txn_dt, label = TRUE, abbr = FALSE),
    txn_month = month(txn_dt, label = TRUE, abbr = FALSE)
  ) |>
  select(-transaction_time)

# Aggregate to daily
daily_revenue <- cafe_txns |>
  mutate(revenue = unit_price * transaction_qty) |>
  group_by(txn_date, store_location) |>
  summarise(
    revenue = sum(revenue),
    transactions = n(),
    .groups = "drop"
  )

# Date boundaries
min_date <- min(daily_revenue$txn_date)
max_date <- max(daily_revenue$txn_date)

# UI
ui <- page_sidebar(
  title = "Café Revenue Forecast Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    selectInput("location", "Location:",
                choices = c("All Locations", unique(daily_revenue$store_location))),
    
    dateRangeInput("date_range", "Forecast Period:",
                   start = max_date + 1,
                   end = max_date + 7,
                   min = max_date + 1,
                   max = max_date + 30),
    
    radioButtons("interval", "Confidence Interval:",
                 choices = c("80%" = 0.80, "95%" = 0.95))
  ),
  
  card(
    card_header("Revenue Forecast"),
    plotOutput("forecast_plot", height = "400px")
  ),
  
  card(
    card_header("Staffing Recommendations"),
    tableOutput("staffing_table")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: Filter data by location
  location_data <- reactive({
    if (input$location == "All Locations") {
      daily_revenue |>
        group_by(txn_date) |>
        summarise(revenue = sum(revenue), .groups = "drop")
    } else {
      daily_revenue |> filter(store_location == input$location)
    }
  })
  
  # Reactive: Train Prophet model
  forecast_data <- reactive({
    # Prepare data
    prophet_data <- location_data() |>
      rename(ds = txn_date, y = revenue)
    
    # Train model
    model <- prophet(
      daily.seasonality = FALSE,
      weekly.seasonality = TRUE,
      yearly.seasonality = FALSE,
      seasonality.mode = 'multiplicative'
    )
    model <- fit.prophet(model, prophet_data)
    
    # Generate forecast
    n_days <- as.numeric(difftime(input$date_range[2], 
                                  input$date_range[1], 
                                  units = "days")) + 1
    
    future <- make_future_dataframe(model, periods = n_days, freq = 'day')
    
    predict(model, future, interval_width = as.numeric(input$interval)) |>
      filter(ds >= input$date_range[1], ds <= input$date_range[2])
  })
  
  # Output: Plot
  output$forecast_plot <- renderPlot({
    data <- forecast_data()
    
    ggplot(data, aes(x = ds, y = yhat)) +
      geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), 
                  fill = "lightblue", alpha = 0.3) +
      geom_line(colour = "darkred", linewidth = 1.2) +
      geom_point(colour = "darkred", size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Predicted Revenue")
  })
  
  # Output: Staffing table
  output$staffing_table <- renderTable({
    forecast_data() |>
      mutate(
        Date = format(ds, "%A, %b %d"),
        Revenue = scales::dollar(round(yhat)),
        `Morning Staff` = ceiling((yhat * 0.6) / 1500),
        `Afternoon Staff` = ceiling((yhat * 0.4) / 1500)
      ) |>
      select(Date, Revenue, `Morning Staff`, `Afternoon Staff`)
  })
}

shinyApp(ui, server)
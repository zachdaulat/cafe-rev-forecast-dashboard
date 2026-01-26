library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(readxl)
library(prophet)
library(lubridate)



# --- Themeing setup ------------------------------------------ ====
source("theme_znord.R")

bs_theme_znord <- bs_theme(
  bootswatch = "darkly",
  
  # Override with Nord colors
  bg = "#2e3440",           # Nord polar night
  fg = "#d8dee9",           # Nord snow storm
  success = "#90bc8f",      # Nord aurora green
  info = "#5e81ac",
  warning = "#ebcb8b",      # Nord aurora yellow
  danger = "#D24B57",       # Nord aurora red
  
  # Fonts
  base_font = font_google("Source Sans 3"),
  code_font = font_google("Fira Code"),
  
  # Spacing
  "card-border-radius" = "8px",
  "card-border-width" = "1px"
)



# --- Data loading and setup ---------------------------------- ====

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





# -------------- SHINY CODE ------------------------------------ ====

# UI
ui <- page_sidebar(
  title = "Café Revenue Forecast Dashboard",
  theme = bs_theme_znord,
  
  sidebar = sidebar(
    h5("Forecast Configuration"),
    
    selectInput("location", "Location:",
                choices = c("All Locations", unique(daily_revenue$store_location))),
    
    dateRangeInput("date_range", "Forecast Period:",
                   start = max_date + 1,
                   end = max_date + 7,
                   min = max_date + 1,
                   max = max_date + 61),
    
    radioButtons("interval", "Confidence Interval:",
                 choices = c("80%" = 0.80, "95%" = 0.95)),
    
    checkboxInput("monthly_seasonality", 
                  "Include monthly seasonality (experimental)", 
                  value = FALSE),
    
    # Forecast button
    actionButton("run_forecast", "Generate Forecast", 
                 class = "btn-primary w-100 mt-3"),
    
    hr(), # Visual separator
    
    h5("Staffing Parameters"),
    
    numericInput("revenue_per_staff", 
                 "Revenue per staff member:",
                 value = 1500,
                 min = 500,
                 max = 3000,
                 step = 100),
    
    sliderInput("morning_pct", 
                "Morning revenue allocation:",
                min = 0.4,
                max = 0.8,
                value = 0.6,
                step = 0.05),
    
    numericInput("shift_hours", 
                 "Shift length (hours):",
                 value = 5,
                 min = 4,
                 max = 8,
                 step = 0.5),
    
    numericInput("min_staff", 
                 "Minimum staff per shift:",
                 value = 2,
                 min = 1,
                 max = 5,
                 step = 1)
  ),
  
  # Forecast summary cart
  card(
    card_header("Forecast Summary"),
    max_height = "250px",
    
    layout_column_wrap(
      width = 1/3,
      
      value_box(
        title = "Total Revenue",
        value = textOutput("total_revenue"),
        showcase = bsicons::bs_icon("currency-dollar"),
        showcase_layout = "left center",
        theme = "primary"
      ),
      
      value_box(
        title = "Peak Day",
        value = textOutput("peak_day"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        showcase_layout = "left center",
        theme = "success"
      ),
      
      value_box(
        title = "Avg Daily",
        value = textOutput("avg_revenue"),
        showcase = bsicons::bs_icon("calculator"),
        showcase_layout = "left center",
        theme = "info"
      )
    )
  ),
  
  # Forecast plot card
  card(
    card_header("Revenue Forecast"),
    plotOutput("forecast_plot", height = "400px")
  ),
  
  # Staffing table card
  card(
    card_header("Staffing Recommendations"),
    max_height = "350px",
    class = "overflow-auto",
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
  
  # eventReactive: Only train model when button is clicked
  forecast_data <- eventReactive(input$run_forecast, {
    
    # Show progress indicator
    withProgress(message = 'Training forecast model...', {
      
      # Prepare data
      prophet_data <- location_data() |>
        rename(ds = txn_date, y = revenue)
      
      incProgress(0.3, detail = "Fitting Prophet model")
      
      # Train model
      model <- prophet(
        daily.seasonality = FALSE,
        weekly.seasonality = TRUE,
        yearly.seasonality = FALSE,
        seasonality.mode = 'multiplicative',
        interval.width = as.numeric(input$interval)
      )
      
      if (input$monthly_seasonality) {
        model <- add_seasonality(
          model,
          name = "monthly",
          period = 30.5,
          fourier.order = 5
        )
      }
      
      model <- fit.prophet(model, prophet_data)
      
      incProgress(0.6, detail = "Generating predictions")
      
      # Generate forecast
      last_train_date <- max(prophet_data$ds)
      n_days <- as.numeric(difftime(input$date_range[2], 
                                    last_train_date, 
                                    units = "days")) + 1
      
      future <- make_future_dataframe(model, periods = n_days, freq = 'day')
      
      predict(model, future) |>
        mutate(ds = as.Date(ds)) |>
        filter(
          ds >= input$date_range[1], 
          ds <= input$date_range[2]
        )
    })
  })
  
  # Output: Plot
  output$forecast_plot <- renderPlot({
    data <- forecast_data()
  
    ggplot(data, aes(x = ds, y = yhat)) +
      geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), 
                  fill = NORD$znord9, alpha = 0.3) +
      geom_line(colour = NORD$znord15, linewidth = 1.2) +
      geom_point(colour = NORD$znord15, size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_znord() +
      labs(
        x = NULL, 
        y = "Predicted Revenue",
        caption = paste0(as.numeric(input$interval) * 100, "% prediction interval")
      )
  })
  
  # Output: Forecast summary info
  output$total_revenue <- renderText({
    req(forecast_data())
    scales::dollar(round(sum(forecast_data()$yhat)))
  })
  
  output$peak_day <- renderText({
    req(forecast_data())
    data <- forecast_data()
    peak <- data |> slice_max(yhat, n = 1)
    format(peak$ds, "%b %d")
  })
  
  output$avg_revenue <- renderText({
    req(forecast_data())
    scales::dollar(round(mean(forecast_data()$yhat)))
  })
  
  # Output: Staffing table
  output$staffing_table <- renderTable({
    req(forecast_data())
    
    forecast_data() |>
      mutate(
        # Calculate staff needed
        morning_staff = pmax(
          ceiling((yhat * input$morning_pct) / input$revenue_per_staff),
          input$min_staff
        ),
        afternoon_staff = pmax(
          ceiling((yhat * (1 - input$morning_pct)) / input$revenue_per_staff),
          input$min_staff
        ),
        
        # Format for display
        Date = format(ds, "%A, %b %d"),
        Revenue = scales::dollar(round(yhat)),
        `Morning Staff` = morning_staff,
        `Afternoon Staff` = afternoon_staff,
        `Total Hours` = (morning_staff + afternoon_staff) * input$shift_hours
      ) |>
      select(Date, Revenue, `Morning Staff`, `Afternoon Staff`, `Total Hours`)
  })
}

shinyApp(ui, server)



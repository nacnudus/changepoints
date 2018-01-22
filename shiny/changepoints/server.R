# Apply a method to a whole corpus of timeseries, namely the London Underground
# performance data.

# This doesn't attempt to update the model or the log with new entries.  It
# reads all entries in one go, and prints plots of the most-recently-detected
# changepoints

# An attempt to update the model and the log is in R/bulk.R

# rsconnect::deployApp(account = "duncan-garmonsway")

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(underground)
library(cpm)
library(shiny)

# Shiny app
# With multiple plots based on https://gist.github.com/wch/5436415/

# Define server function
function(input, output) {
  stat <- "Mood"
  ARL0 <- 100
  startup <- 20
  # Detect all changepoints in all series
  series <-
    underground %>%
    filter(!is.na(period), !is.na(line)) %>%
    inner_join(rail_periods) %>%
    mutate(period = end_date) %>%
    select(metric, line, period, value) %>%
    arrange(period) %>%
    nest(-metric, -line) %>%
    mutate(model = map(data, ~ processStream(.x$value, stat, ARL0, startup))) %>%
    mutate(changePoints = map2(data, model, ~ .x$period[.y$changePoints + 1]),
           detectionTimes = map2(data, model, ~ .x$period[.y$detectionTimes]),
           changeValues = map2(data, model, ~ .x$value[.y$changePoints + 1]))
  # Extract only the changepoints, as a log
  changes <-
    series %>%
    select(-data, -model) %>%
    unnest()
  plot_series <- function(metric, line, changePoints, detectionTimes,
                          changeValues, max_date = Inf, ...) {
    changepoints <-
      tibble(changePoints, detectionTimes, changeValues) %>%
      filter(detectionTimes <= max_date)
      # tibble(changePoints = text_to_period(changePoints),
      #        detectionTimes = text_to_period(detectionTimes),
      #        changeValues)
    .metric <- metric
    .line <- line
    series %>%
      filter(metric == .metric, line == .line) %>%
      select(metric, line, data) %>%
      unnest() %>%
      filter(period <= max_date) %>%
      # mutate(period = text_to_period(period)) %>%
      ggplot(aes(period, value)) +
      geom_line(colour = "grey80") +
      geom_vline(aes(xintercept = changePoints),
                   colour = "red",
                   data = changepoints,
                   linetype = 5) +
      geom_point(aes(x = changePoints, y = changeValues), data = changepoints, colour = "black") +
      geom_segment(aes(x = changePoints, xend = detectionTimes,
                       y = changeValues, yend = changeValues),
                 data = changepoints,
                 colour = "black") +
      ggtitle(.metric, subtitle = .line) +
      xlab("") +
      ylab("") +
      theme(panel.background = element_blank())
  }
  # Subset data
  changes_up_to_date <- reactive({
    req(input$date)
    filter(changes, detectionTimes <= input$date)
  })
  series_to_plot <- reactive({
    req(input$n)
    changes_up_to_date() %>%
      arrange(desc(detectionTimes)) %>%
      slice(seq_len(input$n)) %>%
      distinct(metric, line) %>%
      inner_join(series, by = c("metric", "line"))
  })
  # Create the plots
  plots <- reactive({
    req(input$date)
    pmap(series_to_plot(), plot_series, max_date = input$date)
  })
  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    req(input$n)
    plot_output_list <-
      seq_len(max(input$n, nrow(series_to_plot()))) %>%
      map(~ plotOutput(paste0("plot", .x), height = 280, width = 500))
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  observe({
    req(input$n)
    for (i in seq_len(max(input$n, nrow(series_to_plot())))) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste0("plot", my_i)
        output[[plotname]] <- renderPlot({
          plots()[[my_i]]
        })
      })
    }
  })
}

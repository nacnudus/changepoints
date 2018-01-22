# Apply a method to a whole corpus of timeseries, namely the London Underground
# performance data.

# This doesn't attempt to update the model or the log with new entries.  It
# reads all entries in one go, and prints plots of the most-recently-detected
# changepoints

# An attempt to update the model and the log is in R/bulk.R

library(tidyverse)
library(glue)
library(underground)
library(cpm)
library(lubridate)
library(here)

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

# Print the log, ordered by the detection date, which is the order they would be
# reported
changes %>%
  arrange(detectionTimes, changePoints, metric, line) %>%
  print(n = Inf)

plot_series <- function(metric, line, changePoints, detectionTimes,
                        changeValues, ...) {
  changepoints <- tibble(changePoints, detectionTimes, changeValues)
    # tibble(changePoints = text_to_period(changePoints),
    #        detectionTimes = text_to_period(detectionTimes),
    #        changeValues)
  .metric <- metric
  .line <- line
  series %>%
    filter(metric == .metric, line == .line) %>%
    select(metric, line, data) %>%
    unnest() %>%
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
# Plot the latest detections
latest_changes <- filter(changes, detectionTimes == max(detectionTimes))
plots <-
  latest_changes %>%
  select(metric, line) %>%
  inner_join(series) %>%
  pmap(plot_series)
for (i in seq_along(plots)) {
  cat(i, "\n")
  ggsave(glue("temp/{i}.png"), plots[[i]])
}

# Plot the nth-latest detections
# https://stackoverflow.com/a/4916318/937932
x <-
  changes %>%
  arrange(desc(detectionTimes)) %>%
  pull(detectionTimes)
x_unique <- unique(x)
x_ranks <- rank(x_unique)
changes$order <- as.integer(x_ranks[match(x,x_unique)])
ordered_changes <-
  changes %>%
  arrange(order, changePoints, metric, line)
ordered_changes
plots <-
  ordered_changes %>%
  filter(order == 10) %>%
  select(metric, line) %>%
  inner_join(series) %>%
  pmap(plot_series)
for (i in seq_along(plots)) {
  cat(i, "\n")
  ggsave(glue("temp/{i}.png"), plots[[i]])
}

# Plot n detections up to a date
latest_date <- ymd("2017-12-09")
n <- 10
changes_up_to_date <-
  changes %>%
  filter(detectionTimes <= latest_date)
n_series_to_view <-
  changes_up_to_date %>%
  arrange(desc(detectionTimes)) %>%
  slice(seq_len(n)) %>%
  select(metric, line) %>%
  inner_join(series)
plots <- pmap(n_series_to_view, plot_series)
for (i in sprintf("%02i", seq_along(plots))) {
  cat(glue("temp/{i}.png"), "\n")
  ggsave(glue("temp/{i}.png"), plots[[as.integer(i)]])
}

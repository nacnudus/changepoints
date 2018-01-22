# Apply a method to a whole corpus of timeseries, namely the London Underground
# performance data.

# This was an attempt to maintain both a state and a log, updating both with
# each new observation.

# That proved to be a pain, and not necessary to demonstrate what a log would
# look like.  Another implementation that doesn't attempt incremental updates is
# in R/one-off.R

library(tidyverse)
library(underground)
library(cpm)
library(here)

series <-
  underground %>%
  filter(!is.na(period), !is.na(line)) %>%
  inner_join(rail_periods) %>%
  mutate(period = end_date) %>%
  select(metric, line, period, value) %>%
  arrange(period) %>%
  nest(-metric, -line)

stat <- "Mood"
ARL0 <- 100
startup <- 20

# Warm up the system by applying onstructing the CPM objects on the startup
# number of observations.

blank <- makeChangePointModel(cpmType = stat, ARL0 = ARL0, startup = startup)
blanks <- mutate(series,
                 cpm = list(blank),
                 detection_times = list(integer()),
                 changepoints = list(integer()))

update_with_new_value <- function(.data, cpm, observation) {
  new_value <- slice(.data, observation)$value
  processObservation(cpm, new_value)
}

observations <- blanks
observation <- 0L
for (i in seq_len(startup - 1)) {
  observation <- observation + 1L
  observations <-
    observations %>%
    mutate(cpm = pmap(list(data, cpm, observation), update_with_new_value),
           detected = map_lgl(cpm, changeDetected),
           detection_times = map2(detected,
                                  detection_times,
                                  ~ if(.x) c(.y, observation) else .y),
           changepoints = pmap(list(detected, changepoints, cpm),
                               ~ if(..1) c(..2, which.max(getStatistics(..3))) else ..2))
}
print(observation)
observations %>%
  filter(detected)

# Make a further observation and update the model
observation <- observation + 1L
observations <-
  observations %>%
    mutate(cpm = pmap(list(data, cpm, observation), update_with_new_value),
           detected = map_lgl(cpm, changeDetected),
           detection_times = map2(detected,
                                  detection_times,
                                  ~ if(.x) c(.y, observation) else .y),
           changepoints = pmap(list(detected, changepoints, cpm),
                               ~ if(..1) c(..2, which.max(getStatistics(..3))) else ..2))
print(observation)

observations %>%
  filter(detected)

observations %>%
  filter(detected) %>%
  pull(detection_times)

observations %>%
  filter(detected) %>%
  pull(changepoints)

plot_changepoints <- function(metric,
                              line,
                              data,
                              detection_times,
                              changepoints) {
  plotdata <- slice(data, seq_len(observation))
  changepoints <- tibble(changepoint = plotdata$period[changepoints],
                         detection_time = plotdata$period[detection_times],
                         y = plotdata$value[changepoints])
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_vline(aes(xintercept = changepoint),
                 colour = "red",
                 data = changepoints,
                 linetype = 5) +
    geom_point(aes(x = changepoint, y = y), data = changepoints, colour = "black") +
    geom_segment(aes(x = changepoint, xend = detection_time, y = y, yend = y),
               data = changepoints,
               colour = "black") +
    theme_void()
}

plotseries <-
  observations %>%
  filter(detected) %>%
  slice(1)

plot_changepoints(plotseries$metric,
                  plotseries$line,
                  plotseries$data[[1]],
                  plotseries$detection_times[[1]],
                  plotseries$changepoints[[1]])

metric <- plotseries$metric
line <- plotseries$line
data <- plotseries$data[[1]]
detection_times <- plotseries$detection_times[[1]]
changepoints <- plotseries$changepoints[[1]]

getStatistics(x)

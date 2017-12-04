library(tidyverse)
library(underground)
library(changepoint)
library(animation)
library(here)

# Look for a metric with interesting change points on the Jubilee Line (chosen
# because I know they did some major engineering a few years ago).
underground %>%
  filter(
         # metric == "Train delays longer than 15 minutes",
         !is.na(fourweek),
         line == "Jubilee") %>%
  mutate(colour = underground_colours[line],
         period = as.numeric(str_sub(year, 1L, 4L)) + fourweek / 13) %>%
  select(metric, period, line, colour, value) %>%
  ggplot(aes(period, value, colour = colour)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y")

# Similar but neater, for a slide
underground %>%
  filter(
         # metric == "Train delays longer than 15 minutes",
         !is.na(fourweek),
         line == "Jubilee") %>%
  mutate(colour = underground_colours[line],
         period = as.numeric(str_sub(year, 1L, 4L)) + fourweek / 13) %>%
  select(metric, period, line, colour, value) %>%
  ggplot(aes(period, value)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y") +
  theme_void() +
  theme(strip.text = element_blank(),
        legend.position = "none")
ggsave(here("slides", "too-many-timeseries.png"), height = 5.319335, width = 9.898376, units = "in")


# Interesting series:
# * "Scheduled kilometres"
# * "% of Timetabled Kilometres"
# * "Number of Service Control Failures"
# * "Number Track Failures"
# * "Escalator Availability"
# * "Journes longer than 15 minutes"
# * "Station closures"

skms <-
  underground %>%
  filter(metric == "Scheduled kilometres",
         !is.na(fourweek),
         line == "Jubilee") %>%
  mutate(period = as.numeric(str_sub(year, 1L, 4L)) + fourweek / 13) %>%
  select(period, value)

ggplot(skms, aes(period, value)) +
  geom_line() +
  theme_void()

# Animated version for a slide
plot_points <- function(x, n_obs = nrow(x), ...) {
  plotdata <- slice(x, seq_len(n_obs))
  ggplot(plotdata, aes(period, value)) +
    geom_line() +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
  theme_void()
}
plot_points(skms, 6)

saveGIF(movie.name = here("slides", "guess-changepoint.gif"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_points(skms, i))
    ani.pause()
  }
}, convert = "convert", interval = 0.1)

# Animated with changepoints
plot_changepoints <- function(x, n_obs = nrow(x), ...) {
  plotdata <- slice(x, seq_len(n_obs))
  changepoints <-
    cpt.meanvar(plotdata$value, class = TRUE, ...)
  horizontals <-
    skms %>%
    slice(c(1, cpts(changepoints))) %>%
    rename(x = period) %>%
    mutate(xend = lead(x, default = max(plotdata$period)),
           y = param.est(changepoints)$mean)
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_segment(aes(x = x, xend = xend, y = y, yend = y),
                 colour = "red",
                 data = horizontals) +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
    theme_void()
}
plot_changepoints(skms)
plot_changepoints(skms, 6)

saveGIF(movie.name = here("slides", "changepoints.gif"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_changepoints(skms, i))
    ani.pause()
  }
}, convert = "convert", interval = 0.1)

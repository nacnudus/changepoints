library(tidyverse)
library(underground)
library(changepoint)
library(BreakoutDetection) # devtools::install_github("BreakoutDetection")
library(ecp)
library(animation)
library(gridExtra)
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
plot_twitter <- function(x, n_obs = nrow(x), ...) {
  plotdata <- slice(x, seq_len(n_obs))
  changepoints <- breakout(plotdata$value, method = "multi")
  verticals <-
    skms %>%
    slice(changepoints$loc) %>%
    rename(x = period) %>%
    mutate(xend = x,
           y = -Inf, yend = Inf)
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "red",
                 data = verticals,
                 linetype = 5) +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
    theme_void()
}
print(plot_twitter(skms))
ggsave(here("slides", "twitter-still.png"),
       width = 4,
       height = 4)
print(plot_twitter(skms, 6))

saveGIF(movie.name = here("slides", "twitter.gif"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_twitter(skms, i))
  }
}, convert = "convert", interval = 0.1)

# Compare with the 'changepoints' package
plot_changepoint_twitter <- function(x, n_obs = nrow(x), ...) {
  plotdata <- slice(x, seq_len(n_obs))
  changepoint_changepoints <- cpt.meanvar(plotdata$value, class = TRUE, ...)
  changepoint_verticals <-
    skms %>%
    slice(cpts(changepoint_changepoints)) %>%
    rename(x = period) %>%
    mutate(xend = x,
           y = -Inf, yend = Inf)
  twitter_changepoints <- breakout(plotdata$value, method = "multi")
  twitter_verticals <-
    skms %>%
    slice(twitter_changepoints$loc) %>%
    rename(x = period) %>%
    mutate(xend = x,
           y = -Inf, yend = Inf)
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "red",
                 data = changepoint_verticals,
                 linetype = 5) +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "blue",
                 data = twitter_verticals,
                 linetype = 5) +
    geom_text(aes(max(x - .1, 0, na.rm = TRUE), 850000),
                 colour = "red",
                 data = changepoint_verticals,
                 label = "changepoint",
                 hjust = 1) +
    geom_text(aes(max(x - .1, 0, na.rm = TRUE), 900000),
                 colour = "blue",
                 data = twitter_verticals,
                 label = "twitter",
                 hjust = 1) +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
    theme_void()
}
print(plot_changepoint_twitter(skms, 100))
print(plot_changepoint_twitter(skms))
print(plot_changepoint_twitter(skms, 8))

saveGIF(movie.name = here("slides", "comparison-changepoint-twitter.gif"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_changepoint_twitter(skms, i))
  }
}, convert = "convert", interval = 0.1)

saveVideo(video.name = here("slides", "comparison-changepoint-twitter.mp4"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_changepoint_twitter(skms, i))
  }
}, convert = "convert", interval = 0.1)

# Compare with the 'ecp' package (using the 'agglomerative' method, because it
# is effectively non-parametric).  There are FAR TOO MANY changepoints.  Same
# for the divisive method, unless the time between of changepoints is severely
# restricted to be > 30 periods.
plot_ecp <- function(x, n_obs = nrow(x), ...) {
  plotdata <- slice(x, seq_len(n_obs))
  ecp_changepoints <- e.agglo(matrix(plotdata$value, ncol = 1))
  ecp_verticals <-
    skms %>%
    slice(ecp_changepoints$estimates) %>%
    rename(x = period) %>%
    mutate(xend = x,
           y = -Inf, yend = Inf)
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "forestgreen",
                 data = ecp_verticals,
                 linetype = 5) +
    geom_text(aes(max(x - .1, 0, na.rm = TRUE), 800000),
                 colour = "forestgreen",
                 data = ecp_verticals,
                 label = "ecp",
                 hjust = 1) +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
    theme_void()
}
print(plot_ecp(skms, 100))
print(plot_ecp(skms))
print(plot_ecp(skms, 8))

saveGIF(movie.name = here("slides", "ecp.gif"), {
  for (i in seq_len(nrow(skms - 3)) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_ecp(skms, i))
  }
}, convert = "convert", interval = 0.1)

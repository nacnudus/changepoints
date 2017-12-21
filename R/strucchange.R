library(strucchange)
library(here)

# Animated with changepoints
plot_strucchange <- function(x, n_obs = nrow(x), stat = "Mood",
                             ...) {
  plotdata <- slice(x, seq_len(n_obs))
  changepoints <-
    processStream(plotdata$value, cpmType = stat, ARL0 = ARL0, startup = 20)
  changepoints <-
    tibble(x =    slice(skms, changepoints$changePoints + 1)$period,
           xend = slice(skms, changepoints$detectionTimes)$period,
           y =    slice(skms, changepoints$changePoints)$value)
  ggplot(plotdata, aes(period, value)) +
    geom_line(colour = "grey80") +
    geom_vline(aes(xintercept = x),
                 colour = "red",
                 data = changepoints,
                 linetype = 5) +
    geom_point(aes(x = x, y = y), data = changepoints, colour = "black") +
    geom_segment(aes(x = x, xend = xend, y = y, yend = y),
               data = changepoints,
               colour = "black") +
    xlim(range(skms$period)) +
    ylim(range(skms$value)) +
    theme_void()
}
print(plot_strucchange(skms, stat = "Mann-Whitney"))
print(plot_strucchange(skms, stat = "Mood"))
print(plot_strucchange(skms, stat = "Lepage"))
print(plot_strucchange(skms, stat = "Kolmogorov-Smirnov"))
print(plot_strucchange(skms, stat = "Cramer-von-Mises"))
ggsave(here("slides", "strucchange-still.png"),
       width = 4,
       height = 4)
print(plot_strucchange(skms, 6))

saveGIF(movie.name = here("slides", "strucchange.gif"), {
  for (i in seq_len(nrow(skms) - 3) + 3) {
    ## draw your plots here, then pause for a while with
    cat(i, "\n")
    print(plot_strucchange(skms, i))
  }
}, convert = "convert", interval = 0.1)


# Bug-free version of onlineCPD:::plot.oCPD
plotcpd <- function(x, lines = TRUE, title = "", leg.name = "Variable",
    cleanCP = TRUE, buffer = 10, ...) {

    x <- res
    lines <- TRUE
    title <- ""
    leg.name <- "Variable"
    cleanCP <- TRUE
    buffer <- 10

    value <- variable <- val <- Var1 <- Var2 <- num <- NULL
    T <- dim(x$data)[1]
    R1 <- x$R[1:T, 1:T]
    if (is.null(x$time)) {
        time <- 1:(T)
        melted <- melt(data.frame(cbind(time, data = x$data)),
            id.vars = "time")
        a <- ggplot(data = melted, aes(x = time, y = value, color = variable)) +
            geom_point() + labs(x = "", title = title) + scale_color_discrete(name = leg.name,
            label = colnames(x$data))
        meltedR1 <- melt(R1)
    } else {
        time <- x$time
        melted <- melt(data.frame(cbind(time = time, data = x$data)),
            id.vars = "time")
        melted$time <- as.POSIXct(melted$time, origin = "1970-01-01 00:00:00")
        a <- ggplot(data = melted, aes(x = time, y = value, color = variable)) +
            geom_point() + labs(x = "", title = title) + scale_color_discrete(name = leg.name)
        colnames(R1) <- time
        meltedR1 <- melt(R1)
        meltedR1$Var2 <- as.POSIXlt(meltedR1$Var2, origin = "1970-01-01")
    }
    if (lines) {
      ymin <- ggplot_build(a)$panel$ranges[[1]]$y.range[1]
      ymax <- ggplot_build(a)$panel$ranges[[1]]$y.range[2]
      if (cleanCP) {
        changes <- findCP(x, buffer)
      } else {
        changes <- x$changes
        for (k in changes[-1]) {
          a$layers <- c(geom_segment(data = data.frame(val = k),
                                     aes(x = time[val], xend = time[val], y = ymin,
                                         yend = ymax, alpha = 0.02), color = "red",
                                     show.legend = FALSE), a$layers)
        }
      }
    }
    b <-
      ggplot(data = meltedR1, aes(Var2, Var1, fill = value)) +
      labs(x = "data", y = "runlength") +
      geom_raster() +
      scale_fill_gradient(low = "#FFFFFF",
                          high = "#000000",
                          na.value = "grey92",
                          name = "Probability",
                          trans = log_trans(exp(1)),
                          limits = c(1e-05, 1),
                          breaks = c(1, 0.01, 1e-04)) +
      geom_line(data = data.frame(num = append(time, time[T]),
                                  max = x$max[1:(T + 1)]),
                aes(num, max),
                alpha = 0.5,
                color = "red",
                inherit.aes = FALSE) +
      theme_grey()
    grid.arrange(a, b)

}

library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("lumen"),
  titlePanel("London Underground: Notable Trends"),
  sidebarLayout(
    sidebarPanel(
      # Select date range to be plotted
      dateInput("date",
                "Show changes detected up to this date:",
                value = format(Sys.time(), "%Y-%m-%d"),
                max = format(Sys.time(), "%Y-%m-%d")),
      numericInput("n", "Number of series to show:",
                  value = 10,
                  min = 1,
                  max = 100,
                  step = 1)
    ),
    # Output: Description, lineplot, and reference
    mainPanel(
      uiOutput(outputId = "plots")
    )
  )
)

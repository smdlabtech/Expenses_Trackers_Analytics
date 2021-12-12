# Expenses Trackers 
ET_analytics is an expenses tracking application. He has the particularity of having a date range on which you can choose the period to analyze.

# Table of contents

- [Example](#example)
- [How to use](#usage)
- [Installation](#install)
- [Features](#features)
- [Sponsors 🏆](#sponsors

<h2 id="example">Example</h2>
To see how this application works click on the following link : https://smd-lab-tech.shinyapps.io/ET_Smart_App/

```
library(shiny)

ui <- fluidPage(
    actionButton("go", "Go"),
    shinycssloaders::withSpinner(
        plotOutput("plot")
    )
)
server <- function(input, output) {
    output$plot <- renderPlot({
        input$go
        Sys.sleep(1.5)
        plot(runif(10))
    })
}
shinyApp(ui, server)
```

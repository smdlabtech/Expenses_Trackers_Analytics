# Expenses Trackers 
ET_analytics is an expenses tracking application. He has the particularity of having a date range on which you can choose the period to analyze.

# Table of contents

- [Example](#example)
- [How to use](#usage)
- [Installations](#install)
- [Features](#features)
- [Sponsors 🏆](#sponsors)

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

<h2 id="install">Installations</h2>
```
library(tm)
library(webshot)
library(stringi)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(gtsummary)
library(RColorBrewer)
library(reactable)
library(shinyjs)
library(kableExtra)
library(ggrepel)
library(plotly)
library(IRdisplay)
library(stats)
library(data.table)
library(openxlsx)                #library de ConvertToDate 
library(sp)
library(shiny)
library(leaflet)
library(DT)
library(tibble) 
library(shinydashboard)
#library(shinydashboardPlus)     #developped by young people (for free)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(fpp)
library(sp)
```


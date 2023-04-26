Portfolio 10
================
Conor Lacey
2023-04-26

``` r
suppressWarnings(
  suppressMessages(
    c(
    library(shiny),
    library(tidyverse),
    library(rsconnect)
    )
  )
)
```

    ##  [1] "shiny"     "stats"     "graphics"  "grDevices" "utils"     "datasets" 
    ##  [7] "methods"   "base"      "lubridate" "forcats"   "stringr"   "dplyr"    
    ## [13] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
    ## [19] "shiny"     "stats"     "graphics"  "grDevices" "utils"     "datasets" 
    ## [25] "methods"   "base"      "rsconnect" "lubridate" "forcats"   "stringr"  
    ## [31] "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"  
    ## [37] "tidyverse" "shiny"     "stats"     "graphics"  "grDevices" "utils"    
    ## [43] "datasets"  "methods"   "base"

### Introduction

Alright for a my last portfolio I am going to be making a shiny app for
Eric. Specifically I am going to make a z-distribution that will be able
to shade in the p-values and display the alpha cut-off rejection
regions.

Now shiny is a whole different ball game for r, so I’m going to start
off very simple and work our way up to more difficult tasks for this
project.

### Normal Distribution

Let’s begin by making sure we can create a normal distribution in the
app.

``` r
server <- function(input,output){
  x <- seq(-4,4, length.out = 1e4)
  y <- dnorm(x)
  
  dat <- data.frame( x = x, y = y) #Normal Dist. Data
  
    graph <- dat %>% ggplot(aes(x = x, y = y)) +
    geom_line(color = "blue", linewidth = 1) +
    ylab("Density") +
    xlab("Z") + 
    theme_classic() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 15))
    
    output$distplot <- graph
}

ui <- fluidPage(
  mainPanel(plotOutput("distPlot"))
)

# Run the application 
shinyApp(ui = ui, server = server)
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

library(bslib)
library(plotly)
library(shiny)
library(tidyverse)
library(magrittr)

# 1 Get data ---

tornado <- read_csv("final_reading_tornado.csv")

# UI ----
# Define UI for application that creates the tornado visualization 
ui <- page_fillable(

    # Application title
    titlePanel("SOL Reading Gaps by School Division"),
    
    # Row for user selections 
    fluidRow(
      column(12, align = "center",
             selectInput(inputId = "demo",
                         label = "Select a Demographic to Examine",
                         choices = unique(tornado$demographic),
                         selected = NULL),
             #selectInput(inputID = "division", 
                         #label = "Select a Division to Highlight",
                         #choices = unique(tornado$division_name),
                         #selected = NULL),
             sliderInput("year", "Select Year:", 
                         min = min(tornado$test_year), max = max(tornado$test_year), value = 2022, step = 1, sep =''),
      ),
    ),
    
    # Row for plot 
    fluidRow(
      column(12, align = "center",
             plotlyOutput("tornadoPlot"),
        )
    )
)

# Server ----
# Define server logic required to create the tornado 
server <- function(input, output) {

    output$tornadoPlot <- renderPlotly({

        # draw the plot with the specified demographics and year 
      tornado %>% 
        filter(demographic == input$demo, test_year == input$year) %>% 
        mutate(division_name = fct_reorder(division_name, desc(rank))) %>%
        ggplot(aes(y = division_name, x = rate, group = division_name, color = level)) +
        geom_path(color = "grey") +
        geom_point() +
        labs(x ="", y = "School Division") +  
        theme_classic() +
        theme(axis.text.y = element_text(size = 2))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

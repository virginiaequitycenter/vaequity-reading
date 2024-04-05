library(bslib)
library(plotly)
library(shiny)
library(tidyverse)
library(magrittr)

# 1 Get data ---

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
        filter(demographic == input$demo, test_year == input$year)%>% 
        pivot_wider(names_from = level, values_from = rate) %>%
        ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
        geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
        geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'blue', size = 2) +
        geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'yellow', size = 2) +
        labs(x ="", y = "School Division") +  
        theme_classic() +
        theme(axis.text.y = element_text(size = 2))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

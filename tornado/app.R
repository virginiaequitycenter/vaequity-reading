library(bslib)
library(plotly)
library(shiny)
library(tidyverse)
library(magrittr)

# 1 Get data ---

tornado <- read_csv("final_reading_tornado.csv")

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

tornado$level <- capFirst(tornado$level)

# UI ----
# Define UI for application that creates the tornado visualization 
ui <- page_fillable(
  
  # Application title
  titlePanel(h1(strong("SOL Reading Gaps by School Division"), align = "center"), 
             "SOL Reading Gaps by School Division"),
  
  # Row for user selections 
  fluidRow(
    column(6, align = "center",
           selectInput(inputId = "demo",
                       label = "Select a Demographic to Examine",
                       choices = unique(tornado$demographic),
                       selected = NULL)),
    column(6, align = "center",
           selectInput(inputId = "division",
                       label = "Select a Division to Highlight",
                       choices = unique(tornado$division_name),
                       selected = NULL)),
  ),
  
  # Row for year selection
  fluidRow(
    column(12, align = "center",
           sliderInput("year", "Select Year:", 
                       min = min(tornado$test_year), max = max(tornado$test_year), 
                       value = 2023, step = 1, sep ='')
    )
  ),
  
  # Row for plot 
  fluidRow(
    column(12, align = "center",
           plotlyOutput("tornadoPlot", height='800px'),
    )
  )
)

# Server ----
# Define server logic required to create the tornado 
server <- function(input, output) {
  
  rvs = reactiveValues()
  
  observeEvent(c(input$year, input$demo), {
    rvs$plt_dat = tornado %>% 
      filter(demographic == input$demo, test_year == input$year) %>% 
      mutate(division_name = fct_reorder(division_name, desc(rank)))
  })
  
  output$tornadoPlot <- renderPlotly({
    
    if (input$year == 2020) {
      validate("There was no SOL testing in 2020. Please select another year. ")
    }
    
    tornado_plot <- ggplot(rvs$plt_dat, aes(y = division_name, x = rate, group = division_name, color = level,
                                            text = paste0("Divison: ", division_name, 
                                                          "\nDemographic: ", level,
                                                          "\nPass Rate: ", scales::percent(rate, accuracy = 0.1)))) +
      geom_path(color = "grey") +
      geom_path(data = filter(rvs$plt_dat, division_name == input$division), color = "black", size = 1) +
      geom_point() +
      labs(x ="", y = "School Division") +  
      theme_classic() +
      theme(axis.text.y = element_text(size = 9)) +
      labs(colour = NULL) +
      scale_x_continuous(labels = scales::percent, position = "top") +
      scale_color_manual(values=c("#E69F00", "#56B4E9")) +
      geom_vline(xintercept = c(0.4, 0.6, 0.8), linetype = "dotted", color = "light grey") 
    
    ggplotly(tornado_plot, height = 2000, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

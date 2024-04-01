library(shiny)
library(tidyverse)
library(janitor)
library(plotly)

dumbbell <- read_csv("dumbbell/www/2023_tornado.csv") 

dumbbell_race <- tornado %>% 
  filter(demographic == "Race", test_year == 2022) %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'blue', size = 2) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'yellow', size = 2) +
  labs(x ="", y = "School Division") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))

# Define UI for application 
ui <- fluidPage(
  #tags$h3("Virginia Reading SOL Gaps, 2006-2022"),
  fluidRow(
    column(12, align="center",
           selectInput(inputId = "target",
                       label = "Select a School Division to highlight",
                       choices = unique(dumbbell_race$division_name),
                       selected = NULL),
           tags$p("Hover over a line to see the division, pass rate, and number of third grade students."),
           
    )
  ),
  
  fluidRow(
    column(12,
           
           
           # Line plot
           plotlyOutput("traceplot"),
    )),
  
  tags$p("Data: Virginia Department of Education, https://www.doe.virginia.gov/statistics_reports/sol-pass-rates/index.shtml"),
  tags$p("Note: From 2013 to 2019, the number of students taking the SOL averaged 92,320. In 2021, the number was 72,057."),
  

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

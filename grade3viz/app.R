# Third Grade Reading SOL Pass Rates
# Michele Claibourn
# December 2021



library(shiny)
library(tidyverse)
library(janitor)
library(plotly)

grade3 <- read_csv("www/thirdgrade_alllstudents.csv") 

# generate data frames
grade3_prof <- grade3 %>% 
  clean_names() %>% 
  select(school_year, division_number, division_name, 
         pass_proficient_rate, total_students = total_count) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year)) %>% 
  filter(year > 2012)

grade3_prof_all <- grade3 %>% 
  clean_names() %>% 
  group_by(school_year) %>% 
  summarize(total_pass = sum(pass_proficient_count, na.rm = TRUE),
            total_students = sum(total_count, na.rm = TRUE)) %>% 
  mutate(pass_proficient_rate = round((total_pass/total_students)* 100, 1)) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year)) %>% 
  filter(year > 2012)


# Define UI for application 
ui <- fluidPage(
  tags$h3("Virginia Reading SOL Percent Proficient, 2013-2021"),
  fluidRow(
    column(12, align="center",
           selectInput(inputId = "target",
                       label = "Select a School Division to highlight",
                       choices = unique(grade3_prof$division_name),
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


    # select target
    target <- reactive({
        d1 <- grade3_prof %>% filter(division_name == input$target)
    })

    # generate plot
    output$traceplot <- renderPlotly({
      grade3_prof %>% 
            ggplot(aes(x = year, y = pass_proficient_rate, 
                       label = total_students
                       )) + 
        # line traces for each jurisdiction in all panels
        geom_line(aes(group = division_name),
                  size = 0.2, color = "grey80") +
        # bigger background line reflecting state-wide rate
        geom_line(data = grade3_prof_all, aes(x = year, y = pass_proficient_rate), 
                  color = "black", size = 2) +
        scale_x_continuous(breaks = seq(2013, 2021, 1)) +
        labs(x = "Year (spring)", y = "% Proficient",
             caption = "Data: Virginia Department of Education <br> https://www.doe.virginia.gov/statistics_reports/sol-pass-rates/index.shtml") +
        # line trace in red for target jurisdiction 
        geom_line(data = target(), aes(y = pass_proficient_rate, 
                                       #text = division_name, label = total_students
                                       ), color = "firebrick") +
        # rectangle over 2020 test
        annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 25, ymax = 85,
                 alpha = 1, fill = "white") +
        annotate("text", x = 2020, y = 85, label = "No 2020 Test", size = 3) +
        # label state-wide rate
        annotate("segment", x = 2013.5, xend = 2014.5, y = 28, yend = 28, 
                 color = "black", size = 2) +
        annotate("text", x = 2014, y = 25, label = "State-wide Proficiency",
                 size = 3) +
        theme_minimal() 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

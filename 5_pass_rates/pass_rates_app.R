# Explore 6th Grade Reading and 3rd Grade Math pass rates

library(janitor)
library(plotly)
library(reactable)
library(shiny)
library(tidyverse)

# Get data ----

# Note: Data read in and cleaned in 1_dataprep/data/processed/1... and saved to Box before moving forward
# Here we just copied the data to this folder

math <- read_csv("1_complete_math.csv")
reading <- read_csv("1_complete_reading.csv")

# Prep Math ----

math <- math %>% 
  mutate_at(vars(division_number, pass_count, total_count, pass_rate, test_year, 
                 max_grade,years_from_end, mean_rate,min_rate,average), as.numeric) %>%
  subset(demographic == "Overall")

# *All Pass Rates ----
math_prof <- math %>% 
  clean_names() %>% 
  filter(test_level=="Grade 6") %>% 
  select(school_year, division_number, division_name, 
         pass_rate, total_students = total_count) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

# *Mean Pass Rates ----
grade6_prof_all <- math %>% 
  clean_names() %>%
  filter(test_level=="Grade 6") %>% 
  group_by(school_year) %>% 
  summarize(total_pass = sum(pass_count, na.rm = TRUE),
            total_students = sum(total_count, na.rm = TRUE),
            division_name) %>% 
  mutate(pass_rate = round((total_pass/total_students)* 100, 1)) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

# *Table ----
math_tbl <- math_prof %>% 
  select(c("division_name","pass_rate","year")) %>%
  pivot_wider(names_from=year,values_from=pass_rate) %>%
  add_row(division_name = 'VA Average', !!! round(colMeans(.[-1], na.rm = TRUE), digits = 2))

math_tbl <- t(math_tbl) %>%
  row_to_names(1) %>%
  as.data.frame()

math_tbl[] <- paste0(as.matrix(math_tbl), '%')

# Prep Reading ----

reading <- reading %>% 
  mutate_at(vars(pass_count, total_count, pass_rate, test_year, max_grade, years_from_end,
                 mean_rate,min_rate,average), as.numeric) %>% 
  subset(demographic=="Overall")

# *All Pass Rates ----
read_prof <- reading %>% 
  clean_names() %>% 
  filter(test_level=="Grade 3") %>% 
  select(school_year, division_number, division_name, 
         pass_rate, total_students = total_count) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

# *Mean Pass Rates ----
grade3_prof_all <- reading %>% 
  clean_names() %>%
  filter(test_level=="Grade 3") %>% 
  group_by(school_year) %>% 
  summarize(total_pass = sum(pass_count, na.rm = TRUE),
            total_students = sum(total_count, na.rm = TRUE),
            division_name) %>% 
  mutate(pass_rate = round((total_pass/total_students)* 100, 1)) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

# *Table ----

read_tbl <- read_prof %>% 
  select(c("division_name","pass_rate","year")) %>%
  pivot_wider(names_from=year,values_from=pass_rate) %>%
  add_row(division_name = 'VA Average', !!! round(colMeans(.[-1], na.rm = TRUE), digits = 2)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

read_tbl <- t(read_tbl) %>%
  row_to_names(1) %>%
  as.data.frame()

read_tbl[] <- paste0(as.matrix(read_tbl), '%')

# UI ----

ui <- fluidPage(
  fluidRow(
    column(8, align="center", offset=2,
           tags$h1("Virginia Standards of Learning Pass Rates, 2006-2024")
                  )),
  
  fluidRow(
    column(12, align="center",
           selectInput(inputId = "target",
                       label = "Select a School Division to highlight:",
                       choices = unique(math_prof$division_name),
                       selected = "Charlottesville City"),
           tags$p("Hover over a line to see the division, year, pass rate, and number of students taking SOL test in the given grade."),
    )
  ),
  
  fluidRow(
    column(12,

    # Line plot - Reading
    plotlyOutput("traceplot2"))),
    
    # Line plot - Math
    fluidRow(
      column(12,
    plotlyOutput("traceplot"))),
    
    #Tables
    fluidRow(
      column(6, align="center",
           tags$h4("SOL Pass Rates for 3rd Grade Reading in Virginia"),
           reactableOutput("mytable_reading")),
      column(6, align="center",
           tags$h4("SOL Pass Rates for 6th Grade Math in Virginia"),
           reactableOutput("mytable"))),
  
    fluidRow(
      column(12,
    tags$p("Data: Virginia Department of Education, https://www.doe.virginia.gov/statistics_reports/sol-pass-rates/index.shtml"),
    )))


# Server ----

server <- function(input, output) {
### Calling in the input from the user
   target <- reactive({
        d1 <- math_prof %>% filter(division_name == input$target)})
   
   target2 <- reactive({
     d1 <- read_prof %>% filter(division_name == input$target)})
   
   target_table <- reactive({
     math_tbl[c(input$target, "VA Average")] })
     
  target_table_read <- reactive({
    read_tbl[c(input$target, "VA Average")] })
 
### Creating Plotly for Line Chart - Math
    output$traceplot <- renderPlotly({
      g_prof_math <- 
        ggplot(data=math_prof, aes(x = year, y = pass_rate,label = total_students)) + 
        geom_line(data=math_prof, aes(group = division_name,
                                      text=paste0("Division: ",division_name,"\nYear: ",year,
                                                  "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                  total_students)),
                  size = 0.2, color = "grey") +
        scale_x_continuous(name = "Year (spring)", breaks = seq(2005, 2024, 1)) +
        labs(title="6th Grade Mathematics SOL Percent Passing by Year",y = "% SOL Pass Rates") +
        geom_line(data = target(), aes(x = year, y = pass_rate,group=division_name,label = total_students,
                                     text=paste0("Division: ",division_name,"\nYear: ",year,
                                                 "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                 total_students)),
                  color = "firebrick") +
        geom_line(data = grade6_prof_all, aes(x = year, y = pass_rate,group=division_name,label = total_students,
                                              text=paste0("All Virginia Students","\nYear: ",year,
                                                          "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                          total_students)),
                  color = "black", size=1.5) +
        annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 1, ymax = 100,
                 alpha = 1, fill = "grey90") +
        annotate("text", x = 2020, y = 50, label = "No\n2020\nTest", size = 8, color="#8c8c8c") +
        annotate("segment", x = 2007, xend = 2007.75, y = 25, yend = 25, 
                 color = "black", size = 2) +
        theme_minimal()
      
      ggplotly(g_prof_math, tooltip = "text" ) %>%
        layout(annotations = list(x = 2008, y = 18.5,
                                  text = "State-wide Proficiency",
                                  showarrow = F))

    })
    
### Creating Reactable for Table
    output$mytable <- renderReactable({
      reactable(target_table(), # we needed target_table() instead of target_table
                defaultPageSize = 18, showPagination = FALSE) 
    })
    
    output$mytable_reading <- renderReactable({
      reactable(target_table_read(), # we needed target_table() instead of target_table
                defaultPageSize = 18, showPagination = FALSE) 
    })
    
### Creating Plotly for Line Chart - Reading
    output$traceplot2 <- renderPlotly({
      g_prof_read <- 
        ggplot(data=read_prof, aes(x = year, y = pass_rate,label = total_students)) + 
        geom_line(data=read_prof, aes(group = division_name,
                                      text=paste0("Division: ",division_name,"\nYear: ",year,
                                                  "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                  total_students)),
                  size = 0.2, color = "grey") +
        scale_x_continuous(name = "Year (spring)", breaks = seq(2005, 2024, 1)) +
        labs(title="3rd Grade Reading SOL Percent Passing by Year",y = "% SOL Pass Rates") +
        geom_line(data = target2(), aes(x = year, y = pass_rate,group=division_name,label = total_students,
                                       text=paste0("Division: ",division_name,"\nYear: ",year,
                                                   "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                   total_students)),
                  color = "firebrick") +
        geom_line(data = grade3_prof_all, aes(x = year, y = pass_rate,group=division_name,label = total_students,
                                              text=paste0("All Virginia Students","\nYear: ",year,
                                                          "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                          total_students)),
                  color = "black", size=1.5) +
        annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 1, ymax = 100,
                 alpha = 1, fill = "grey90") +
        annotate("text", x = 2020, y = 50, label = "No\n2020\nTest", size = 8, color="#8c8c8c") +
        annotate("segment", x = 2007, xend = 2007.75, y = 25, yend = 25, 
                 color = "black", size = 2) +
        theme_minimal()
      
      ggplotly(g_prof_read, tooltip = "text" ) %>%
        layout(annotations = list(x = 2008, y = 18.5,
                                  text = "State-wide Proficiency",
                                  showarrow = F))
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

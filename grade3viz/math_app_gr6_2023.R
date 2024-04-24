##############################################################
# Math and Reading SOL Pass Rates by County over Time- App               
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-04-22    
# Summary: Pull graphs and tables together into Shiny app for
#          publication on Equity Atlas
##############################################################

library(shiny)
library(tidyverse)
library(janitor)
library(plotly)
library(reactable)

math <- read_csv("2005_2023_math_all_students_all_divisions.csv")
reading <- read_csv("2005_2023_reading_all_students_all_divisions.csv")


##############################################################
# Data Wrangling - Math                              
##############################################################

colnames(math) <- c("school_year", "division_number","division_name","subject",
                       "test_level", "test_source", "test", "pass_advanced_count",
                       "pass_proficient_count","fail_count","pass_count","total_count",
                       "pass_advanced_rate","pass_proficient_rate","fail_rate",
                       "pass_rate","avg_sol_scaled")

math <- math %>% mutate_at(c(8:17), as.numeric)

### All Pass Rates by School District and Year

math_prof <- math %>% 
  clean_names() %>% 
  filter(test_level=="Grade 6") %>% 
  select(school_year, division_number, division_name, 
         pass_rate,pass_proficient_rate, total_students = total_count) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

### Mean Pass Rates of all VA School Districts by Year

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

### Making Visual Table

math_tbl <- math_prof %>% select(c("division_name","pass_rate","year"))
math_tbl <- math_tbl %>% pivot_wider(names_from=year,values_from=pass_rate)

math_avg <- grade6_prof_all %>% group_by(year) %>% summarise(va_pass_rate=mean(pass_rate))
math_avg <- t(math_avg)
math_avg <- math_avg %>% row_to_names(1)
math_avg <- as.data.frame(math_avg)
math_avg <- math_avg %>% add_column(division_name="Virginia State")

math_tbl <- merge(math_tbl,math_avg, all.x = TRUE, all.y=TRUE)
math_tbl <- t(math_tbl)
math_tbl <- math_tbl %>% row_to_names(1)
math_tbl <- as.data.frame(math_tbl)
math_tbl <- cbind(rownames(math_tbl), data.frame(math_tbl, row.names=NULL))
colnames(math_tbl)[which(names(math_tbl) == "rownames(math_tbl)")] <- "Year"

math_tbl[] <- paste0(as.matrix(math_tbl), '%')
math_tbl$Year<-gsub("%","",as.character(math_tbl$Year))

##############################################################
# Data Wrangling - Reading                        
##############################################################

colnames(reading) <- c("school_year", "division_number","division_name","subject",
                    "test_level", "test_source", "test", "pass_advanced_count",
                    "pass_proficient_count","fail_count","pass_count","total_count",
                    "pass_advanced_rate","pass_proficient_rate","fail_rate",
                    "pass_rate","avg_sol_scaled")

reading <- reading %>% mutate_at(c(8:17), as.numeric)

### All Pass Rates by School District and Year

read_prof <- reading %>% 
  clean_names() %>% 
  filter(test_level=="Grade 3") %>% 
  select(school_year, division_number, division_name, 
         pass_rate,pass_proficient_rate, total_students = total_count) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.integer(year))

### Mean Pass Rates of all VA School Districts by Year

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

### Making Visual Table

read_tbl <- read_prof %>% select(c("division_name","pass_rate","year"))
read_tbl <- read_tbl %>% pivot_wider(names_from=year,values_from=pass_rate)

read_avg <- grade3_prof_all %>% group_by(year) %>% summarise(va_pass_rate=mean(pass_rate))
read_avg <- t(read_avg)
read_avg <- read_avg %>% row_to_names(1)
read_avg <- as.data.frame(read_avg)
read_avg <- read_avg %>% add_column(division_name="Virginia State")

read_tbl <- merge(read_tbl,read_avg, all.x = TRUE, all.y=TRUE)
read_tbl <- t(read_tbl)
read_tbl <- read_tbl %>% row_to_names(1)
read_tbl <- as.data.frame(read_tbl)

##############################################################
# Define UI for Application                            
##############################################################

ui <- fluidPage(
  fluidRow(
    column(8, align="center", offset=2,
           tags$h2("Virginia Standards of Learning Pass Rates, 2006-2023"),
           tags$p("The Virginia Standards of Learning (SOL) assessments establish the state's expectations for student learning at the end of each 
                  grade in core subjects.The SOL measures the success of the student in retaining concepts from their core courses as well as the 
                  school district's ability to impart those concepts. Students in Virginia take this exam in May from 3rd to 12th grade. Below, we 
                  have compiled pass rates for SOL assessments in Reading and Mathematics for core years."),
           
           tags$p("As detailed in an Annie E. Casey report, third grade reading is a critical educational milestone marking a transition from 
                  `learning to read` to `reading to learn` and low proficiency by the end of third grade has a detrimental impact on a child's future.
                  As such, SOL scores in third grade are of particular importance for educators and administrators. For this reason, we have provided 
                  Reading SOL pass rates for the third grade across Virginia school districts from 2006 to 2023 below. For information on other grades,
                  data can be collected from the Virginia Department of Education's website."),
           
           tags$p("After the 6th grade, students are placed into different math classes in accordance with their retention of previous years' concepts
                  as understood through grades and SOL scores. As such, 6th grade SOL scores have particular relevance to the educational future of Virginia's
                  students. For this reason, we have provided Mathematics SOL pass rates for the sixth grade across Virginia school districts from 2006 to 2023 below.
                  For information on other grades,data can be collected from the Virginia Department of Education's website. "),
                  )),
  
  fluidRow(
    column(12, align="center",
           selectInput(inputId = "target",
                       label = "Select a School Division to highlight:",
                       choices = unique(math_prof$division_name),
                       selected = NULL),
           tags$p("Hover over a line to see the division, year, pass rate, and number of students taking SOL test in the given grade."),
    )
  ),
  
  fluidRow(
    column(12,

    # Line plot - Reading
    plotlyOutput("traceplot2"),
    
    # Line plot - Math
    plotlyOutput("traceplot"),
    
    #Table - Math
    reactableOutput("table")
    
    )),
  
    tags$p("Data: Virginia Department of Education, https://www.doe.virginia.gov/statistics_reports/sol-pass-rates/index.shtml"),

    )


##############################################################
# Server Logic for Line Plot and Table                         
##############################################################
server <- function(input, output) {
### Calling in the input from the user
   target <- reactive({
        d1 <- math_prof %>% filter(division_name == input$target)})
   
   target2 <- reactive({
     d1 <- read_prof %>% filter(division_name == input$target)})

   
### Creating Plotly for Line Chart - Math
    output$traceplot <- renderPlotly({
      g_prof_math <- 
        ggplot(data=math_prof, aes(x = year, y = pass_rate,label = total_students)) + 
        geom_line(data=math_prof, aes(group = division_name,
                                      text=paste0("Division: ",division_name,"\nYear: ",year,
                                                  "\nSOL Pass Rate: ",pass_rate,"%\nTotal Students: ",
                                                  total_students)),
                  size = 0.2, color = "grey") +
        scale_x_continuous(name = "Year (spring)", breaks = seq(2005, 2023, 1)) +
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
        annotate("segment", x = 2007, xend = 2007.75, y = 18.5, yend = 18.5, 
                 color = "black", size = 2) +
        theme_minimal()
      
      ggplotly(g_prof_math, tooltip = "text" ) %>%
        layout(annotations = list(x = 2009, y = 18.5,
                                  text = "State-wide Proficiency",
                                  showarrow = F))

    })
    
### Creating Reactable for Table

    output$table <- renderReactable({
      reactable(math_tbl,minRows = 17,
                columns = list(`Virginia.State` = colDef(name = "All Virginia Students", show =T),
                                Year = colDef(name = "Year", show=T)),
                          defaultColDef = colDef(show = F))
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
        scale_x_continuous(name = "Year (spring)", breaks = seq(2005, 2023, 1)) +
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
        annotate("segment", x = 2007, xend = 2007.75, y = 18.5, yend = 18.5, 
                 color = "black", size = 2) +
        theme_minimal()
      
      ggplotly(g_prof_read, tooltip = "text" ) %>%
        layout(annotations = list(x = 2009, y = 18.5,
                                  text = "State-wide Proficiency",
                                  showarrow = F))
      
    })
    
}




# Run the application 
shinyApp(ui = ui, server = server)

## app.R ##

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)


health <- readRDS("./data/health.rds")

names(health) <- tolower(names(health))
names(health) <- gsub(pattern = "\\.{1,2}", replacement = "_", names(health))
names(health) <- gsub(pattern = "(\\w*)_\\b", "\\1", names(health))

health <- health %>% mutate(start = as.POSIXct(start, format='%d-%b-%Y %H:%M'), 
                            finish = as.POSIXct(finish, format='%d-%b-%Y %H:%M'))

health_sub <- health %>% filter(heart_rate_count_min >= 50) # get rid off zero-heart rate
health_sub <- health_sub %>% filter(resting_calories_kcal <= 1000)
health_sub <- health_sub %>% filter(active_calories_kcal > 0 & resting_calories_kcal > 0)
health_sub <- health_sub %>% select(-finish)

health_sub <- health_sub %>% mutate(hour = hour(start), day = wday(start, label = T, abbr = F), month = month(start, label = T, abbr = F), year = year(start))
health_sub$year <- as.factor(health_sub$year)


source("./data/theme_matt.R", local = TRUE)

ui <- dashboardPage(
        dashboardHeader(title = "Health Analyzer", titleWidth = 250),
        dashboardSidebar(sidebarMenu(
                menuItem("Introduction/Example", tabName = "intro", icon = icon("book", lib = "glyphicon")),
                menuItem("UserData", tabName = "user", icon = icon("user", lib = "glyphicon"))
        )),
        dashboardBody(tags$head(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))),
                      tabItems(tabItem(
                              tabName = "intro",
                              fluidPage(
                                      titlePanel(title = "Example/Intro"),
                                      sidebarPanel(helpText("This 'health' analyser uses iOS data from the iPhone or Apple Watch. Using the iPhone-app 'QS Access', you can access the health data on your iOS Health app and download it as a csv-file. Under 'UserData', you can upload your own csv-file. Your data will not be stored anywhere! Subset data to dig deeper."),
                                                   tags$hr(),
                                                   selectizeInput("years", "Years:", choices = health_sub$year, multiple = T, options = list(placeholder = "Select year(s)", searchConjunction = "or", closeAfterSelect = TRUE)),
                                                   selectizeInput("months", "Months:", choices = health_sub$month, multiple = T, options = list(placeholder = "Select month(s)", searchConjunction = "or", closeAfterSelect = TRUE)),
                                                   selectizeInput("days", "Days of the week:",
                                                                  choices = list("Sunday" = "Sunday", "Monday" = "Monday", "Tuesday" = "Tuesday",
                                                                                 "Wednesday" = "Wednesday", "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday"),
                                                                                 multiple = TRUE, options = list(placeholder = "Select day(s) of the week!", searchConjunction = "or",
                                                                                                                 closeAfterSelect = TRUE)),
                                                   sliderInput(inputId = "hour",
                                                               label = "Hour",
                                                               min = 0,
                                                               max = 23,
                                                               value = c(0, 23), step = 1)
                                                   ),
                                                   
                                      mainPanel(fluidRow(
                                              valueBoxOutput("heart1"),
                                              valueBoxOutput("heart_max1"),
                                              valueBoxOutput("rest1"),
                                              valueBoxOutput("active1"),
                                              valueBoxOutput("dist1"),
                                              valueBoxOutput("steps1")
                                              
                                      ),
                                      fluidRow(
                                                tabBox(title = "", side = "left", selected = "Plots",
                                                  width = 12,
                                                  tabPanel("Plots", plotOutput("plot1"),
                                                           selectizeInput(inputId = "plot1option", label = "Plot options:", 
                                                                          choices = list("Heart_rate/Calories" = "Heart_rate/Calories", "Steps/Calories" = "Steps/Calories"))),
                                                tabPanel("Data table", dataTableOutput("table1"))

                                                      
                                              )
                                      )))),
                                tabItem(
                                        tabName = "user",
                                        fluidPage(titlePanel("Upload your own data here:"),
                                                  sidebarPanel(
                                                          # Input: Select a file ----
                                                          fileInput("file1", "Choose CSV File",
                                                                    multiple = FALSE,
                                                                    accept = c("text/csv",
                                                                               "text/comma-separated-values,text/plain",
                                                                               ".csv")),
                                                          # Horizontal line ----
                                                          tags$hr(),
                                                          # Input: Select separator ----
                                                          
                                                          radioButtons("sep", "Separator",
                                                                       choices = c(Comma = ",",
                                                                                   Semicolon = ";",
                                                                                   Tab = "\t"),
                                                                       selected = ","),
                                                          # Horizontal line ----
                                                          
                                                  tags$hr(),
                                                  helpText("Subset data to drill down"),
                                                  selectizeInput("years2", "Years:", choices = health_sub$year, multiple = T, options = list(placeholder = "Select year(s)", searchConjunction = "or", closeAfterSelect = TRUE)),
                                                  selectizeInput("months2", "Months:", choices = health_sub$month, multiple = T, options = list(placeholder = "Select month(s)", searchConjunction = "or", closeAfterSelect = TRUE)),
                                                  selectizeInput("days2", "Days of the week:",
                                                                 choices = list("Sunday" = "Sunday", "Monday" = "Monday", "Tuesday" = "Tuesday",
                                                                                "Wednesday" = "Wednesday", "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday"),
                                                                 multiple = TRUE, options = list(placeholder = "Select day(s) of the week!", searchConjunction = "or",
                                                                                                 closeAfterSelect = TRUE)),
                                                  sliderInput(inputId = "hour2",
                                                              label = "Hour",
                                                              min = 0,
                                                              max = 23,
                                                              value = c(0, 23), step = 1)),
                                                  
                                                  mainPanel(
                                                          fluidRow(
                                                          valueBoxOutput("heart2"),
                                                          valueBoxOutput("heart_max2"),
                                                          valueBoxOutput("rest2"),
                                                          valueBoxOutput("active2"),
                                                          valueBoxOutput("dist2"),
                                                          valueBoxOutput("steps2")),
                                                          fluidRow(
                                                                  tabBox(title = "", side = "left", selected = "Plots",
                                                                         width = 12,
                                                                         tabPanel("Plots", plotOutput("plot2"),
                                                                                  selectizeInput(inputId = "plot2option", label = "Plot options:", 
                                                                                                 choices = list("Heart_rate/Calories" = "Heart_rate/Calories", "Steps/Calories" = "Steps/Calories"))),
                                                                         tabPanel("Data table", dataTableOutput("table2"))))

                                                                )
                                                )
                      ))
))

server <- function(input, output) {
        
        
# Get reactive output
        
        reactiveOutput <- reactive({
                
                slider_value <- c(min(input$hour), max(input$hour))
                
                health_sub %>% 
                        filter(hour >= slider_value[1] & hour <= slider_value[2])

        }, label = "reactive1")
        
        output$heart1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                
                valueBox(value = round(mean(reactiveOutput()$heart_rate_count_min),2), subtitle = "Heart rate [mean]", 
                         icon = icon("heart", lib = "glyphicon"), col = "red")
                }
                
                else{
                        heart <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarize(heart = mean(heart_rate_count_min))
                        
                        valueBox(value = round(heart$heart, 2), subtitle = "Heart rate [mean]", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                
                }
                
        })
        
        output$heart_max1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        valueBox(value = round(max(reactiveOutput()$heart_rate_count), 2), subtitle = "Max heart rate", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                }
                
                else{
                        heart_max1 <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarise(max = max(heart_rate_count_min))
                        
                        valueBox(value = round(heart_max1$max, 2), subtitle = "Max heart rate", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                }
        })
        
        output$rest1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        valueBox(value = formatC(sum(reactiveOutput()$resting_calories_kcal), big.mark = ",", format = "f", digits = 2), subtitle = "Rest calories", 
                                 icon = icon("dashboard", lib = "glyphicon"), col = "orange")
                }
                
                else{
                        rest_cal <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarize(heart = sum(resting_calories_kcal))
                        
                        valueBox(value = formatC(unlist(rest_cal), big.mark = ",", format = "f", digits = 2), subtitle = "Rest calories", 
                                 icon = icon("dashboard", lib = "glyphicon"), col = "orange")
                }
        })
        
        output$active1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        valueBox(value = formatC(sum(reactiveOutput()$active_calories_kcal), big.mark = ",", format = "f", digits = 2), subtitle = "Active calories", 
                                 icon = icon("fire", lib = "glyphicon"), col = "green")
                }
                
                else{
                        act_cal <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarize(heart = sum(active_calories_kcal))
                        
                        valueBox(value = formatC(unlist(act_cal), big.mark = ",", format = "f", digits = 2), subtitle = "Active calories", 
                                 icon = icon("fire", lib = "glyphicon"), col = "green")
                }
        })
        
        output$dist1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        valueBox(value = formatC(sum(reactiveOutput()$distance_mi), big.mark = ",", format = "f", digits = 2), subtitle = "Distance miles", 
                                 icon = icon("road", lib = "glyphicon"), col = "aqua")
                }
                
                else{
                        dist1 <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarize(sum = sum(distance_mi))
                        
                        valueBox(value = formatC(unlist(dist1), big.mark = ",", format = "f", digits = 2), subtitle = "Distance miles", 
                                 icon = icon("road", lib = "glyphicon"), col = "aqua")
                }
        })
        
        output$steps1 <- renderValueBox({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        valueBox(value = formatC(sum(reactiveOutput()$steps_count), big.mark = ",", format = "f", digits = 2), subtitle = "Steps", 
                                 icon = icon("play", lib = "glyphicon"), col = "yellow")
                }
                
                else{
                        steps1 <- reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% summarize(heart = sum(steps_count))
                        
                        valueBox(value = formatC(unlist(steps1), big.mark = ",", format = "f", digits = 2), subtitle = "Steps", 
                                 icon = icon("play", lib = "glyphicon"), col = "yellow")
                }
        })
        
        ## Plot1
        
        output$plot1 <- renderPlot(
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                
                        if(input$plot1option == "Heart_rate/Calories"){
                
                        reactiveOutput() %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                        ggplot(aes(heart_rate_count_min, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") + 
                                        ylab("Calories") + 
                                        xlab("Heart rate") + 
                                        theme_matt()
                         }
                
                        else{
                        
                        reactiveOutput() %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                ggplot(aes(steps_count, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") +
                                        ylab("Calories") +
                                        xlab("Steps") + 
                                        theme_matt()
                }}
                
                else{
                        if(input$plot1option == "Heart_rate/Calories"){
                                
                                reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(heart_rate_count_min, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") + 
                                        ylab("Calories") + 
                                        xlab("Heart rate") + 
                                        theme_matt()
                        }
                        
                        else{
                                
                                reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days) %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(steps_count, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") +
                                        ylab("Calories") +
                                        xlab("Steps") + theme_matt()
                        }
                }
        )
        output$table1 <- renderDataTable({
                
                if(is.null(input$years) & is.null(input$months) & is.null(input$days)){
                        
                        reactiveOutput()
                }
                
                else{
                        reactiveOutput() %>% filter(year %in% input$years & month %in% input$months & day %in% input$days)
                        
                }
        }, options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 15, 30)))
  
## User table input processing

        
# Get reactive outputII for user
        
        
        reactiveOutput2 <- reactive({
                
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, head of that data file by default,
                # or all rows if selected, will be shown.
                
                req(input$file1)
                
                user_health <- read.csv(input$file1$datapath,
                                        header = TRUE,
                                        sep = input$sep)
                
                
                names(user_health) <- tolower(names(user_health))
                names(user_health) <- gsub(pattern = "\\.{1,2}", replacement = "_", names(user_health))
                names(user_health) <- gsub(pattern = "(\\w*)_\\b", "\\1", names(user_health))
                
                user_health <- user_health %>% mutate(start = as.POSIXct(start, format='%d-%b-%Y %H:%M'), 
                                                      finish = as.POSIXct(finish, format='%d-%b-%Y %H:%M'))
                
                user_health_sub <- user_health %>% filter(heart_rate_count_min >= 50) # get rid off zero-heart rate
                user_health_sub <- user_health_sub %>% filter(resting_calories_kcal <= 1000)
                user_health_sub <- user_health_sub %>% filter(active_calories_kcal > 0 & resting_calories_kcal > 0)
                user_health_sub <- user_health_sub %>% select(-finish)
                
                user_health_sub <- user_health_sub %>% mutate(hour = hour(start), day = wday(start, label = T, abbr = F), month = month(start, label = T, abbr = F), year = year(start))
                user_health_sub$year <- as.factor(user_health_sub$year)
                
                slider_value2 <- c(min(input$hour2), max(input$hour2))
                
                user_health_sub %>% 
                        filter(hour >= slider_value2[1] & hour <= slider_value2[2])
                
        }, label = "reactive2")
        
        output$heart2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = round(mean(reactiveOutput2()$heart_rate_count_min),2), subtitle = "Heart rate [mean]", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                }
                
                else{
                        heart2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarize(heart = mean(heart_rate_count_min))
                        
                        valueBox(value = round(heart2$heart, 2), subtitle = "Heart rate [mean]", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                        
                }
                
        })
        
        output$heart_max2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = round(max(reactiveOutput2()$heart_rate_count), 2), subtitle = "Max heart rate", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                }
                
                else{
                        heart_max2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarise(max = max(heart_rate_count_min))
                        
                        valueBox(value = round(heart_max2$max, 2), subtitle = "Max heart rate", 
                                 icon = icon("heart", lib = "glyphicon"), col = "red")
                }
        })
        
        output$rest2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = formatC(sum(reactiveOutput2()$resting_calories_kcal), big.mark = ",", format = "f", digits = 2), subtitle = "Rest calories", 
                                 icon = icon("dashboard", lib = "glyphicon"), col = "orange")
                }
                
                else{
                        rest_cal2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarize(heart = sum(resting_calories_kcal))
                        
                        valueBox(value = formatC(unlist(rest_cal2), big.mark = ",", format = "f", digits = 2), subtitle = "Rest calories", 
                                 icon = icon("dashboard", lib = "glyphicon"), col = "orange")
                }
        })
        
        output$active2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = formatC(sum(reactiveOutput2()$active_calories_kcal), big.mark = ",", format = "f", digits = 2), subtitle = "Active calories", 
                                 icon = icon("fire", lib = "glyphicon"), col = "green")
                }
                
                else{
                        act_cal2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarize(heart = sum(active_calories_kcal))
                        
                        valueBox(value = formatC(unlist(act_cal2), big.mark = ",", format = "f", digits = 2), subtitle = "Active calories", 
                                 icon = icon("fire", lib = "glyphicon"), col = "green")
                }
        })
        
        output$dist2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = formatC(sum(reactiveOutput2()$distance_mi), big.mark = ",", format = "f", digits = 2), subtitle = "Distance miles", 
                                 icon = icon("road", lib = "glyphicon"), col = "aqua")
                }
                
                else{
                        dist2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarize(sum = sum(distance_mi))
                        
                        valueBox(value = formatC(unlist(dist2), big.mark = ",", format = "f", digits = 2), subtitle = "Distance miles", 
                                 icon = icon("road", lib = "glyphicon"), col = "aqua")
                }
        })
        
        output$steps2 <- renderValueBox({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        valueBox(value = formatC(sum(reactiveOutput2()$steps_count), big.mark = ",", format = "f", digits = 2), subtitle = "Steps", 
                                 icon = icon("play", lib = "glyphicon"), col = "yellow")
                }
                
                else{
                        steps2 <- reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% summarize(heart = sum(steps_count))
                        
                        valueBox(value = formatC(unlist(steps2), big.mark = ",", format = "f", digits = 2), subtitle = "Steps", 
                                 icon = icon("play", lib = "glyphicon"), col = "yellow")
                }
        })
        
        ## Plot1
        
        output$plot2 <- renderPlot(
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        if(input$plot2option == "Heart_rate/Calories"){
                                
                                reactiveOutput2() %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(heart_rate_count_min, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") + 
                                        ylab("Calories") + 
                                        xlab("Heart rate") + 
                                        theme_matt()
                        }
                        
                        else{
                                
                                reactiveOutput2() %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(steps_count, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") +
                                        ylab("Calories") +
                                        xlab("Steps") + 
                                        theme_matt()
                        }}
                
                else{
                        if(input$plot2option == "Heart_rate/Calories"){
                                
                                reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(heart_rate_count_min, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") + 
                                        ylab("Calories") + 
                                        xlab("Heart rate") + 
                                        theme_matt()
                        }
                        
                        else{
                                
                                reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2) %>% gather(`active_calories_kcal`, `resting_calories_kcal`, key = calories, value = measure) %>% 
                                        ggplot(aes(steps_count, measure, color = calories)) + geom_jitter(alpha = .6) + geom_smooth() +
                                        scale_color_manual(values = c("tomato", "steelblue"), labels = c("Active", "Resting"), name = "Calories") +
                                        ylab("Calories") +
                                        xlab("Steps") + theme_matt()
                        }
                }
        )
        output$table2 <- renderDataTable({
                
                if(is.null(input$years2) & is.null(input$months2) & is.null(input$days2)){
                        
                        reactiveOutput2()
                }
                
                else{
                        reactiveOutput2() %>% filter(year %in% input$years2 & month %in% input$months2 & day %in% input$days2)
                        
                }
        }, options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 15, 30)))

}

shinyApp(ui, server)
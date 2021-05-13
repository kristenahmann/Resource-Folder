#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

data <- read.csv("athletes.csv")

olympics <- data[,-1]
olympics$sport <- as.factor(olympics$sport)
olympics$nationality <- as.factor(olympics$nationality)
olympics$sex <- as.factor(olympics$sex)

olympics <- olympics %>% mutate(total_medals = gold+silver+bronze)

ui <- fluidPage(
    
    # give your page a title
    titlePanel("Rio Olympics Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "sport",
                label = "Sport",
                levels(olympics$sport),
                multiple = FALSE
            ),
            selectInput(
                inputId = "sex",
                label = "Sex",
                levels(olympics$sex),
                multiple = FALSE
            )
        ),
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("extra_info")
        )
    )
)


server <- function(input, output){
    output$distPlot <- renderPlot({
        dat <- olympics %>% filter(sport == input$sport) %>% filter(sex == input$sex)
        dat$total_medals <- as.factor(dat$total_medals)
        ggplot(dat, aes(x = height, y = weight)) +
            geom_point(aes(color = total_medals)) +
            geom_smooth(method = "lm") +
            labs(title = "Athlete Height & Weight")
    })
    
    output$extra_info <- renderPrint({
        dat_2 <- olympics %>% filter(sport == input$sport) %>% filter(sex == input$sex) %>% filter(total_medals > 0)
        dat_2 <- dat_2 %>% select(name, nationality, height, weight, total_medals)
        dat_2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




















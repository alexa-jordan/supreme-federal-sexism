#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggforce)
library(readr)
library(tidyverse)
library(readxl)
library(tidytext)
library(ggpubr) 
library(rvest)
library(ggthemes)
library(lubridate)
library(RcppArmadillo)

nomination_data <- read_excel("nomination_data.xlsx") %>% 
    mutate(party = ifelse(Party == "Republican", 
                          "Republican Nominee", 
                          "Democratic Nominee")) %>% 
    mutate(gender = ifelse(Gender == "F", 
                           "Female", 
                           "Male")) %>% 
    mutate(female = ifelse(gender == "Female", 
                           1, 
                           0)) %>% 
    mutate(equal = ifelse(Party == Maj_Party, 
                          "Presidential Party Matches Senate Majority", 
                          "Presidential Party Does Not Match Senate Majority"))
nomination_data2 <- nomination_data %>% 
    filter(!Yes == "n/a") %>% 
    mutate(year = as.numeric(year(Date))) %>% 
    mutate(affirm = as.numeric(Yes)) %>% 
    filter(!affirm == 0)

d <- read_csv("SCDB_2020_01_justiceCentered_Citation.csv")

ui <- navbarPage(
    "Final Project Title",
    tabPanel("Home", 
             titlePanel("Welcome to my Page!"),
             h3("Overview of Project"),
             p("General summary of my project."),
             mainPanel(plotOutput("basic_plot")),
             verbatimTextOutput("fit_obj"),
             h3("Navigating This Page"),
             p("My name is Alexa and I study Government",
               a("Connecting to News Source", href = "https://www.cnn.com"),
               "You can reach me at alexa_jordan@college.harvard.edu.")),
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(d)),
                 selectInput("y", "Y variable", choices = names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             plotOutput("polar_plot"),
             h3("Project Background and Motivations"),
             p("Hello, and welcome to my final project for Gov50"),
             p("In his confirmation hearing, Chief Justice Roberts proclaimed 
             that he was an merely an umpire: his job is to call the strikes and
             the balls. He purports to have no political motivations. Justices 
             on the Supreme Court are nominated and confirmed to be the fair 
             applicators of the law. In an ideal world, they are not politicians
             in robes. But does the public buy it? After all, nominees to the 
             highest court in the United States are selected by the leader of a
             political party, the President. Further, the Senate votes to 
             confirm this nominee."),
             p("Our politics are more polarized than ever before. During the 
             presidency of Donald J. Trump, the court has had two vacancies. 
             Both Senate confirmation hearing were vicious and intense. Trump 
             recently said of Justice Kavanaugh that 'I have never 
             seen any human being, and I’m not just talking about Supreme Court… 
             I have never seen a human being treated so badly with false 
             accusations and everything else. I have never seen anything like 
             it.' The question is, have we?"),
             p("In the following project, I will explore the transcripts and 
               voting record of the US Senate. What can we learn about the state
               of the Union by evaluating how these justices are treated during
               their confirmation hearing? A special focus will be placed upon
               the female justices and minority justices who have served on the
               bench. What role does sexism, racism, political polarization, and 
               religious affliation play into the news coverage and content of 
               the Senate Judiciary Committee's confirmation hearings?"),
             h3("About Me"),
             p("My name is Alexa and I study Government. I am passionate about 
             issue of sexism in politics as well as the influence 
             You can reach me at alexa_jordan@college.harvard.edu."))
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter(),
               column = geom_col()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
    
    output$basic_plot <- renderPlot({
        ggplot(nomination_data, aes(gender)) + 
            geom_bar() +
            facet_wrap(~ party) +
            theme_light() +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            theme(strip.background = element_rect(fill = "palevioletred3")) +
            labs(title = "Overview of Supreme Court Nominees by Party \n 1971-2020", 
                 x = "Gender of Nominee", 
                 y = "Number")
    })
    
    output$polar_plot <- renderPlot({
        ggplot(nomination_data2, aes(year, affirm)) +
            geom_point() + 
            facet_wrap(~equal) +
            geom_smooth(method = "lm", se = FALSE, color = "palevioletred3") + 
            theme_light() +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            theme(strip.background = element_rect(fill = "palevioletred3")) +
            labs(y = "Affirmative Votes", 
                 x = "Year (1971-2020)", 
                 title = "Political Polarization in Supreme Court Senate Confirmations")
    })
    output$fit_obj <- renderPrint(
        stan_glm(data = nomination_data, 
                            female ~ party, 
                            family = gaussian(), 
                            refresh = 0)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

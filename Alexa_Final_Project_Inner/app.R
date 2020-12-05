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
library(rstanarm)
library(gt)
library(gtsummary)


######################### SUPREME COURT NOMINEE DATA ##########################


# This first section uses data about the Supreme Court nominees since the 
# appointment of Sandra Day O'Connor in 1981. 


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

# What are the odds that the next supreme court nominee will be a woman?  

fit_obj <- stan_glm(data = nomination_data, 
                    female ~ party - 1, 
                    family = binomial(), 
                    refresh = 0)

# Further wrangling of the nomination_data for another graph. 

nomination_data2 <- nomination_data %>% 
    filter(!Yes == "n/a") %>% 
    mutate(year = as.numeric(year(Date))) %>% 
    mutate(affirm = as.numeric(Yes)) %>% 
    filter(!affirm == 0)


######################### FEDERAL COURT NOMINEES ##############################


judges <- read_rds("judges.rds")

female_nominees <- judges %>% 
    filter(gender == "Female")

judges$female <- 0
judges$female[which(judges$gender == "Female")] <- 1

federal_model <- stan_glm(female ~ party + president,
         data = judges, 
         family = binomial(), 
         refresh = 0) 

ui <- navbarPage(
    "Supreme Sexism", 
    tabPanel("Overview", 
             titlePanel("Women in Federal Court"),
             h3("Project Overview", 
                style = "font-family: 'Palantino'"),
             p("In 2020, we celebrate the centennial of the 19th amendment. 
               This project will confront just one of the many remaining 
               barriers to women in public life. "), 
             p("In 1981, the first female Supreme Court justice Sandra Day 
               O’Connor was appointed by Republican President Ronald Reagan. 
               Women in the legal profession has been growing rapidly, with 
               women in 2016 making up the majority of incoming law school 
               classes. This number is encouraging, but the question 
               still remains:"),
             p("What progress have we made? Where are we going? Are female 
               judges subjected to sexism in the selection and confirmation 
               process for federal judgeships? How have judicial politics 
               adapted to this modern age? Although the scope of this project 
               is necessarily limited, we will look at the data for the 
               Supreme Court, US Court of Appeals, and Circuit Court 
               Presidential nominees since 1981."),
             p("In the following project, I will explore the transcripts and 
               voting record of the US Senate. What can we learn about the state
               of the Union by evaluating how these justices are treated during
               their confirmation hearing? A special focus will be placed upon
               the female justices and minority justices who have served on the
               bench. What role does sexism, racism, political polarization, and 
               religious affliation play into the news coverage and content of 
               the Senate Judiciary Committee's confirmation hearings?"),
             
             plotOutput("over_time"), 
             p("In the above graph, we can see all Presidential confirmations to 
               a federal bench since 1981. There are large cycles of red and blue,
               representing the transferring governments The peaks represent a 
               time period in which Presidents were able to smoothly confirm 
               more justices, whereas the troughs demonstrate alternative 
               priorities or difficulty in the Senate."),
             
             gt_output(outputId = "stat"),
             
             plotOutput("women_nominees"),
             
             gt_output(outputId = "model_2"),
             
             p("This is a model that looks at the impact of the party 
               and the president on the gender of federal court nominees. This
               regression found that Republicans were much less likely to 
               nominate women for the federal bench overall, but Donald J
               Trump actually outperformed Bill Clinton. All Presidents have 
               been at least somewhat less likely to nominate a woman"),
             
             style = "font-family: 'Palantino'"),
    
    tabPanel("Supreme Court Nominees",
              fluidPage(
                  plotOutput("basic_plot"),
                  p("The above graph focuses just on Supreme Court nominees: 
                  Democrats have appointed more women than men, while 
                  Republicans have appointed many more men to the highest court 
                  in the land. The graph below demonstrates the cyclical nature 
                  of new female federal judges. The gap between Republicans and 
                  Democrats is stark. "), 
                  selectInput("choose_president", "President", 
                              choices = levels(judges$president)),
                  plotOutput("plot"), 
                  p("The trends do not really seem to be getting better. 
                    The statistical analysis of these trends does not bode well 
                    either for women on the Supreme Court.",
                    style = "font-family: 'Palantino'"), 
                  gt_output(outputId = "model_tbl")
                  
              ), style = "font-family: 'Palantino'"),
    tabPanel("State of the Union",
             titlePanel("Polarization"),
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
             h3("Setting the Scene"), 
             plotOutput("polar_plot"),
             p("The US in trouble!", 
               style = "font-family: 'Palantino'"), 
             style = "font-family: 'Palantino'"),
    tabPanel("About", 
             titlePanel("About"),
             
             h3("Meet the Creator"),
             
             p("Alexa Jordan is a junior in Adams House with a passion for social 
             justice and career aspirations in law. At Harvard College, Alexa 
             studies Government and Economics. She is interested in studying 
             sexism in politics. The oldest sister of four siblings, Alexa resides 
             in Illinois. 
             She can be contacted by email at alexa_jordan@college.harvard.edu."),
             
             h3("Final Project Inspiration"), 
             
             p("In every imaginable dimension, RBG is a woman I aspire to emulate, 
               whether it be her work with the ACLU in the 1970’s, her relationship 
               with her family members, or her ingenious strategy to shift the 
               dialogue about women’s rights and gender equality in Craig v Boren.  
               Born to a Jewish family in New York, RBG was made acutely aware of 
               justice and equality, or the lack thereof, at an early age. Coming 
               from a background of a religious minority and being a woman, RBG 
               faced significant discrimination as a law student and young lawyer. 
               Her resiliency and perseverance through countless personal and 
               professional setbacks, including illness and blatant sexism, blazed 
               a trial for young women like myself to enter the legal profession. 
               In her book, My Own Words, RBG relayed that, “as women achieve 
               power, the barriers will fall. As society sees what women can do, 
               as women see what women can do, there will be more women out there 
               doing things, and we’ll all be better off for it.” There is no 
               question that RBG’s leadership has impacted my vision of what 
               success looks like for a woman in law."),
             
             p("A change in the rule of law comes first: culture follows suit. 
               Ruth Bader Ginsburg is a woman that I hope we continue to learn 
               from as a Supreme Court justice, lawyer, politician, 
               parent, and scholar. We will all be better off for it."), 
             
             p("My final project is dedicated to her memory."),
             
             p("I have used publically available data",
               a("You can check out my data sources here", 
                 href = "https://www.senate.gov/legislative/nominations/SupremeCourtNominations1789present.htm"),
               "You can reach me at alexa_jordan@college.harvard.edu."),
             style = "font-family: 'Palantino'"
            
             )
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
        judges %>% 
            filter(date >= as.Date("1981-08-19") & date <= as.Date("2020-11-01")) %>% 
            mutate(year = year(date)) %>% 
            filter(president %in% input$choose_president) %>% 
            group_by(year, gender, president) %>% 
            summarise(totals = n(), .groups = "drop") %>% 
            ggplot(aes(x = year, y = totals, color = gender)) + 
            geom_line() +
            theme(text = element_text(family = "Palatino")) +
            labs(title = "Federal Court Nominees by President", 
                 x = "Time", 
                 y = "Count") +
            scale_color_manual(name = "Gender", 
                               values = c("#cd7a68", "#6889cd"), 
                               labels = c("Female", "Male"))
    }, res = 96)
    
    output$basic_plot <- renderPlot({
        ggplot(nomination_data, aes(gender)) + 
            geom_bar() +
            facet_wrap(~ party) +
            theme_light() +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            theme(strip.background = element_rect(fill = "#6889cd")) +
            labs(title = "Overview of Supreme Court Nominees by Party \n 1971-2020", 
                 x = "Gender of Nominee", 
                 y = "Number")
    })
    
    output$polar_plot <- renderPlot({
        ggplot(nomination_data2, aes(year, affirm)) +
            geom_point() + 
            facet_wrap(~equal) +
            geom_smooth(method = "lm", se = TRUE, color = "#6889cd") + 
            theme_light() +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            theme(strip.background = element_rect(fill = "#6889cd")) +
            labs(y = "Affirmative Votes", 
                 x = "Year (1971-2020)", 
                 title = "Political Polarization in Supreme Court Senate Confirmations")
    })
    output$fit_obj <- renderPrint(
        stan_glm(data = nomination_data, 
                            female ~ party, 
                            family = gaussian(), 
                            refresh = 0))
    
    output$over_time <- renderPlot({
        ggplot(judges, aes(x = date, fill = party)) + 
            geom_histogram(color = "white", binwidth = 200) +
            scale_fill_manual(name = "Nominating Party", 
                              values = c("#6889cd", "#cd7a68"), 
                              labels = c("Democrat", "Republican")) +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            labs(title = "Presidential Nominees to Federal Courts", 
                 subtitle = "From 1981 to 2020", 
                 x = "Date", 
                 y = "Count") 
        
    })
    output$stat <- render_gt({
        judges %>%
        group_by(party, gender) %>%
        summarize(judges = n(), .groups = "drop") %>%
        gt() %>% 
        cols_label(party = "Party",
                   gender = "Gender",
                   judges = "Number of Judges") %>%
        tab_header(title = "Breakdown of Federal Judges") %>% 
        tab_style(cell_borders(sides = "right"),
                  location = cells_body(columns = vars(party))) %>%
        tab_style(cell_text(weight = "bold"),
                  location = cells_body(columns = vars(judges))) %>%
        cols_align(align = "center", columns = TRUE) %>%
        fmt_markdown(columns = TRUE)
        })
    
    output$model_tbl <- render_gt({
        tbl_regression(fit_obj, intercept = TRUE) %>%  
            as_gt %>% 
            tab_header(title = "What will be the gender of the next nominee?")
    })
    
    output$model_2 <- render_gt({
        tbl_regression(federal_model, intercept = TRUE) %>% 
            as_gt() %>% 
            tab_header(title = "Presidents and Parties: 
                       Nominating Female Justices")
    })
    
    output$women_nominees <- renderPlot({
        ggplot(female_nominees, aes(x = date, fill = party)) + 
            geom_histogram(color = "white", binwidth = 300) +
            scale_fill_manual(name = "Nominating Party", 
                              values = c("#6889cd", "#cd7a68"), 
                              labels = c("Democrat", "Republican")) +
            theme(text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 10)) +
            labs(title = "Women Confirmed to Federal Courts", 
                 subtitle = "(1981 to 2020)", 
                 x = "Date", 
                 y = "Count") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(readxl)
library(tidytext)
library(ggpubr) 
library(tidyverse)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(pdftools)
library(gt)
library(ggthemes)
library(lubridate)

nomination_data <- read_excel("alexa-project/nomination_data.xlsx") %>% 
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

plot2 <- ggplot(nomination_data, aes(gender)) + 
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
  
fit_obj <- stan_glm(data = nomination_data, 
                    female ~ party, 
                    family = gaussian(), 
                    refresh = 0)

nomination_data2 <- nomination_data %>% 
  filter(!Yes == "n/a") %>% 
  mutate(year = as.numeric(year(Date))) %>% 
  mutate(affirm = as.numeric(Yes)) %>% 
  filter(!affirm == 0)

plot3 <- ggplot(nomination_data2, aes(year, affirm)) +
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

plot3
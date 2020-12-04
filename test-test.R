library(tidytext)
library(ggpubr) 
library(tidyverse)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(pdftools)
library(lubridate)
library(janitor)
library(readr)

d <- glm(female ~ party + president + race, 
         data = judges, 
         family = "binomial")


summary(d)



judges <- read_csv("Alexa_Final_Project_Inner/judges.csv", 
                   col_types =
                     cols('Gender' = col_character(), 
                          'Race or Ethnicity' = col_character(), 
                          'Confirmation Date (1)' = col_date(),
                          'Party of Appointing President (1)' = col_character(),
                          'Court Type (1)' = col_character(),
                          'Appointing President (1)' = col_character(),
                          'School (1)' = col_character(), 
                          'School (2)' = col_character(), 
                          'Ayes/Nays (1)' = col_character())) %>% 
  clean_names() %>% 
  select(gender, 
         race_or_ethnicity, 
         court_type_1, 
         appointing_president_1, 
         party_of_appointing_president_1, 
         ayes_nays_1, 
         confirmation_date_1, 
         school_1, 
         school_2) %>% 
  rename(
    c("race" = "race_or_ethnicity"), 
    c("type" = "court_type_1"), 
    c("president" = "appointing_president_1"), 
    c("party" = "party_of_appointing_president_1"), 
    c("ayes_nays" = "ayes_nays_1"), 
    c("date" = "confirmation_date_1"), 
    c("undergrad" = "school_1"), 
    c("law_school" = "school_2")) %>% 
  filter(date >= as.Date("1981-08-19") & date <= as.Date("2020-11-01")) %>% 
  mutate(president = as.factor(president)) %>% 
  mutate(date2 = format(date, format = "%Y")) %>% 
  mutate(year = as.numeric(date2))

judges$female <- 0
judges$female[which(judges$gender == "Female")] <- 1

write_rds(judges, "judges.rds")

# Writing a regression 
library(rstanarm)

glm(female ~ party + president + year,
         data = judges) %>% 
  tidy()


stan_glm(female ~ year + party + law_school, 
         data = judges, 
         refresh = 0, 
         family = binomial())

library(MASS)

temp_model1 <- glm(female ~ year + president, 
                   data = judges, 
                   family = "binomial")
summary(temp_model1)
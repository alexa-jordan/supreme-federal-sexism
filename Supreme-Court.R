library(tidytext)
library(ggpubr) 
library(tidyverse)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(pdftools)


library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

files <- list.files(pattern = "pdf$")
opinions <- lapply(files, pdf_text)


url <- c("https://www.congress.gov/111/chrg/shrg56940/CHRG-111shrg56940.htm", 
         "https://www.congress.gov/109/chrg/shrg27916/CHRG-109shrg27916.htm", 
         "https://www.congress.gov/115/chrg/CHRG-115shrg28638/CHRG-115shrg28638.htm")


hearings <- c()
contents <- c()
for(i in 1:length(url)){ 
  
  cong_wbpg <- read_html(url[i])
  body <- cong_wbpg %>%
    html_nodes("pre") %>%
    html_text()
  contents = append(contents, body)
  
  cong_wbpg <- read_html(url[i])
  hearing <- cong_wbpg %>%
    html_node("title") %>%
    html_text()  
  hearings = append(hearings, rep(hearing,each=length(body)))
  
}

hearing_text <- data.frame(hearing = hearings, text = contents)

hearing_words <- hearing_text %>%
  unnest_tokens(word, text) %>%
  count(hearing, word, sort = TRUE) 

filtered_words <- hearing_words %>% 
  filter(word == "family" | word == "women" | word == "woman" | word ==  "children" |
           word == "father" | word == "mother")

total_words <- hearing_words %>% 
  group_by(hearing) %>% 
  summarize(total = sum(n))
  
hearing_words_data <- left_join(filtered_words, total_words) %>% 
  mutate(word_proportions = n/total)

hearing_words_data

plot1 <- ggplot(hearing_words_data, aes(word, word_proportions, fill = hearing)) +
  geom_col(show.legend = FALSE, position = "dodge") 
    
plot1

  +
  facet_wrap(~hearing, ncol = 2, scales = "free_y")


freq_by_rank <- hearing_words_data %>%
  group_by(hearing) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank


freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = hearing)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


hearing_words_data <- hearing_words_data %>%
  bind_tf_idf(word, hearing, n)

hearing_words_data %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(hearing) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = hearing)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hearing, ncol = 2, scales = "free") +
  coord_flip() 


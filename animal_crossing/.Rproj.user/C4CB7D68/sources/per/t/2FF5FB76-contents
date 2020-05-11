source("libraries_&_data.R")


villagers_count <- villagers %>% 
  group_by(personality, song)

%>% 
  ggplot(aes(personality, count_song)) +
  geom_col() 
  
           




# -------------------------------------------------------------------------


review_text <- 
  tibble(
  text = user_reviews$text,
  date = user_reviews$date
) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(date) %>%
  summarise(mean_sentiment = mean(value))

review_text_sentiments %>% 
  ggplot() +
  aes(x = date, y = mean_sentiment, colour ) +
  geom_line() +
  geom_smooth() 

#   -----------------------------------------------------------------------
unique(review_text$value)

review_text <- 
  tibble(
    text = user_reviews$text,
    date = user_reviews$date
  ) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  left_join(get_sentiments("afinn")) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  # left_join(get_sentiments("bing"), by = "word") %>% 
  # mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  group_by(date) %>%
  summarise(mean_value = mean(value)) %>% 
  mutate(sentiment = ifelse(mean_value < 0, 
                            "negative", 
                            ifelse(mean_value == 0, 
                                   "neutral", 
                                   "positive")))

review_text %>% 
  ggplot() +
  aes(x = date, 
      y = mean_value, 
      colour = sentiment) +
  geom_col() 



























img_file <- system.file(file.path("isabelle_background.png"),
                        package = "ggpubr")
img <- readPNG(img_file)





villagers %>% 
  drop_na() %>% 
  group_by(song) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))




all_stops <- c(
  "nintendo", 
  "animal", 
  "crossing", 
  "experience",	
  "multiplayer",	
  "multiple",
  "progress",
  "everyone",
  "horizons",
  "everything",
  "anything",
  "actually",
  "something",
  "absolutely", 
  "gameplay", 
  "together", 
  "completely", 
  "crafting",
  "different",
  "resources",
  stopwords("en"))

review_words <- removeWords(user_reviews$text, all_stops)

frequent_terms <- review_words %>% 
  freq_terms(top = 20)

quarantine <- str_count(user_reviews$text, "quarantine")

sum(quarantine)

quarantine_reviews <- user_reviews %>% 
  drop_na() %>% 
  filter(grepl("quarantine", text))
  
villager_names <- villagers$name

# named_in_review <- user_reviews %>% 
#   filter(grepl(villager_names, text))
## Warning message:
##   In grepl(villager_names, text) :
##   argument 'pattern' has length > 1 and only the first element will be used

quarantine_reviews %>% 
  ggplot(aes(grade)) +
  geom_bar()



nooks_items <- items %>% 
  drop_na(buy_value, sell_value) %>% 
  filter(sources == "Nook's Cranny") 



villagers %>% 
  group_by(species) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(species, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip()
  



user_grades <- user_reviews %>%
  drop_na() %>% 
  ggplot(aes(grade)) +
  geom_bar() +
  theme_bw()

ggplotly(user_grades)


critic_rating_over_time <- critic %>% 
  group_by(date) %>% 
  summarise(avg_grade = mean(grade)) %>% 
  ggplot(aes(date, avg_grade)) +
  geom_line() +
  background_image(img) +
  labs(
    x = "Date",
    y = "Average Critic rating (%)"
  ) 

ggplotly(critic_rating_over_time)


user_rating_over_time <- user_reviews %>%
  group_by(date) %>% 
  summarise(avg_grade = (mean(grade))) %>% 
  ggplot(aes(date, avg_grade)) +
  geom_line() +
  labs(
    x = "Date",
    y = "User rating (0-10)"
  )
ggplotly(user_rating_over_time)






critic_grades <- critic %>%
  drop_na() %>% 
  mutate(grade_category = ifelse(
    grade < 10, 0, 
    ifelse(grade < 20, 1, 
           ifelse(grade < 30, 2,
                  ifelse(grade <= 40, 3,
                         ifelse(grade <= 50, 4,
                                ifelse(grade <= 60, 5,
                                       ifelse(grade <= 70, 6,
                                              ifelse(grade <= 80, 7,
                                                     ifelse(grade <= 90, 8,
                                                            ifelse(grade < 100, 9, 10)
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
    ) %>% 
  ggplot(aes(grade_category)) +
  geom_bar() +
  theme_bw()

ggplotly(critic_grades)

bunny_day_end_reviews <- critic %>% 
  filter(date == "2020-04-13" | date == "2020-04-12")

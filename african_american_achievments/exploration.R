library(tidyverse)

firsts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

summary(firsts)
firsts %>% 
  filter(category == "Arts & Entertainment") %>% 
  ggplot(aes(x = year, )) +
  geom_bar()

science %>% 
  #filter(category == "Arts & Entertainment") %>% 
  ggplot(aes(x = occupation_s)) +
  geom_bar()

science %>% 
  group_by(occupation_s) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

science %>% 
  filter(occupation_s == "Inventor") %>% 
  select(inventions_accomplishments)


firsts %>% 
  #filter(year > 2008) %>% 
  ggplot(aes(x = year)) +
  geom_bar(col = "black", fill = "white") +
  facet_wrap(~category, nrow = 2) +
  theme_classic()

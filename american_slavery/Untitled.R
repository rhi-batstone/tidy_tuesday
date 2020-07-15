library(tidyverse)

blackpast <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

census %>% 
  group_by(year) %>% 
  summarise(count = sum(black_slaves)) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line()

total_black_slaves <- sum(census$black_slaves)

census %>% 
  drop_na() %>% 
  group_by(division) %>% 
  summarise(percentage = (sum(black_slaves)/total_black_slaves)*100) %>% 
  ggplot(aes(x = division, y = percentage)) +
  geom_col(col = "black") +
  theme_classic()
  
  
  

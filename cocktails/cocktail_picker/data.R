library(tidyverse)

# Get the Data
cocktails <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv') %>% 
  rename(name = "drink")
boston_cocktails <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

#creating common variale names
column_names <- names(boston_cocktails)

#trimming cocktails columns
cocktails_match <- cocktails %>% 
  select(column_names)

#joining the two datasets
all_cocktails <- cocktails_match %>% 
  bind_rows(boston_cocktails) %>% 
  select(-row_id)

ingredients <- unique(all_cocktails$ingredient)

boston_cocktails %>% 
  mutate(str_to_lower(ingredient)) %>% 
  filter(str_detect(ingredient, "bitters")) %>% 
  count(ingredient, sort = T)
  
  
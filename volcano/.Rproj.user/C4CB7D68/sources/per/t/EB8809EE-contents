library(tidyverse)
library(leaflet)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

# Creating volcano icon for leaflet plot
volcano_icon <- makeIcon(
  iconUrl = "nature (1).svg",
  iconWidth = 19, iconHeight = 48,
  iconAnchorX = 0, iconAnchorY = 0
)

#making volcano data year if eruption numeric
volcano <- volcano %>%  
  mutate(last_eruption_year = (as.numeric(as.character(last_eruption_year))))


# Leaflet plot
volcano %>%  
  #To reduce the number of icons on the map
  filter(last_eruption_year > 1800) %>% 
  leaflet() %>%
  # A nice minimal map 
  addProviderTiles(
    providers$CartoDB.Positron
    ) %>%
  addMarkers(
    lat = ~latitude,
    lng = ~longitude,
    #volcano shaped markers
    icon = volcano_icon,
    # hover over labels as name and year of eruption
    label = ~paste0(volcano_name, ", ", last_eruption_year)
  )

## Figuring out how to map Japan using geojson file. 
## following this tutorial: https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2/

## Japan Prefecture
library(rvest)
library(janitor)

## scrape data from Wikipedia Page
japan.pref <- read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan") %>%
  html_nodes(".wikitable") %>% html_table(fill=T)
japan.pref <-japan.pref[[2]]

japan.pref <- japan.pref %>% 
  mutate(id = as.integer(str_remove(str_extract(ISO,"\\d+"),"^0"))) %>%
  clean_names()

library(geojsonio)
japan.map <- geojson_read("https://raw.githubusercontent.com/chichacha/land/master/japan.geojson", what = "sp")

library(broom)

## Out of tidy is always a data.frame with disposable row names...  
japan.map.fortify <- japan.map %>% 
  tidy(region = "id") %>% 
  mutate(id=as.integer(id)) %>%
  left_join(japan.pref %>% select(id, region))
japan.map.data <- japan.map@data %>% arrange(id) %>%
  left_join(japan.pref %>% select(id, prefecture, kanji, capital, region, iso))


japan.map.fortify %>% 
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=factor(id))) +
  theme_void(base_family="Osaka") +
  coord_map() +
  scale_fill_manual(values=pref.colour[1:47], 
                    labels=paste0(japan.map.data$nam_ja," (",japan.map.data$nam,")"),
                    name="Prefecture")

ggsave(filename="images/JapanPrefectureColouring.png", width=16, height=12)

japan.map.fortify %>% 
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=fct_inorder(region)), color="#00000080") +
  theme_void(base_family="Roboto Condensed") +
  coord_map() +
  scale_fill_brewer(name="8 Regions in Japan", palette="Set2")

ggsave(filename="images/JapanRegions.png", width=16, height=9)


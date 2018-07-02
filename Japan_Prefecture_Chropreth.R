## Figuring out how to map Japan using geojson file. 
## following this tutorial: https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2/

## Japan Prefecture
library(rvest)

japan.pref <- read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan") %>%
  html_nodes(".wikitable") %>% html_table(fill=T)
japan.pref <-japan.pref[[2]]

library(geojsonio)
japan.map <- geojson_read("https://raw.githubusercontent.com/chichacha/land/master/japan.geojson", what = "sp")

library(broom)
japan.map.fortify <- japan.map %>% tidy(region = "id") %>% mutate(id=as.integer(id))
japan.map.data <- japan.map@data %>% arrange(id)


japan.map.fortify %>% 
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=factor(id))) +
  theme_void(base_family="Osaka") +
  coord_map() +
  scale_fill_manual(values=pref.colour[1:47], 
                    labels=paste0(japan.map.data$nam_ja," (",japan.map.data$nam,")"),
                    name="Prefecture")

ggsave(filename="images/JapanPrefectureColouring.png", width=16, height=12)

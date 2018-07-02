

## Data Preps
library(tidyverse)
library(readxl)


## What sheets are in Excel File?
travel.by.transport <-excel_sheets(path = "Data/2016_Transport_OD.xlsx")


## Below function will read section of sheets where it contains from > to data between prefectures
read_pref_od <- function(path,sheet,...) {
  sheet <-read_excel(path=path, 
                     sheet=sheet, 
                     skip=7, col_names=F)
  
  m <- sheet[4:51,4:51]
  m <- m %>% transmute_all(as.numeric)
  m <- as.matrix(m)

  #pref.jp <- sheet[4:51,3][[1]]
  ## Put Names to the Matrix
  ## Japan Prefectures in English
  pref.en <- c('Hokkaido', 'Aomori', 'Iwate', 'Miyagi', 'Akita', 'Yamagata', 
               'Fukushima', 'Ibaraki', 'Tochigi', 'Gunma', 'Saitama', 'Chiba', 
               'Tokyo', 'Kanagawa', 'Niigata', 'Toyama', 'Ishikawa', 
               'Fukui', 'Yamanashi', 'Nagano', 'Gifu', 'Shizuoka', 'Aichi', 
               'Mie', 'Shiga', 'Kyoto', 'Osaka', 'Hyōgo', 'Nara', 'Wakayama', 
               'Tottori', 'Shimane', 'Okayama', 'Hiroshima', 'Yamaguchi', 
               'Tokushima', 'Kagawa', 'Ehime', 'Kochi', 'Fukuoka', 'Saga', 
               'Nagasaki', 'Kumamoto', 'Oita', 'Miyazaki', 'Kagoshima', 'Okinawa', 
               'Unknown')
  
  colnames(m) <- pref.en
  rownames(m) <- pref.en
  
  ## I like Tables instead of Matrix
  m.df <- as.tibble(m)
  m.df$origin <- pref.en
  m.df <- m.df %>% select(origin, everything())
  m.df.long <- m.df %>% gather(key="destination", value="value", -origin)
  
  m.df.long

}

travel.by.transport

### Read 3 Files.
all.transport.2014 <- read_pref_od(path="Data/2014_Transport_OD.xlsx", sheet=1)
all.transport.2015 <- read_pref_od(path="Data/2015_Transport_OD.xlsx", sheet=1)
all.transport.2016 <- read_pref_od(path="Data/2016_Transport_OD.xlsx", sheet=1)

### 
all.transport <-bind_rows("2014" = all.transport.2014, 
          "2015" = all.transport.2015, 
          "2016" = all.transport.2016, .id = "year")

all.transport$year <- as.integer(all.transport$year)


head(all.transport)

library(tweenr)
library(circlize)

## Set of 48 Colours...  Last one is grey for Unknown
pref.colour <- c("#BA68C8", "#9C27B0", "#7B1FA2", "#7E57C2", "#5E35B1", "#4527A0", 
                 "#7986CB", "#3F51B5", "#303F9F", "#42A5F5", "#1E88E5", "#1565C0", 
                 "#4FC3F7", "#03A9F4", "#0288D1", "#26C6DA", "#00ACC1", "#00838F", 
                 "#4DB6AC", "#009688", "#00796B", "#66BB6A", "#43A047", "#2E7D32", 
                 "#AED581", "#8BC34A", "#689F38", "#D4E157", "#C0CA33", "#9E9D24", 
                 "#FFF176", "#FFEB3B", "#FBC02D", "#FFCA28", "#FFB300", "#FF8F00", 
                 "#FFB74D", "#FF9800", "#F57C00", "#FF7043", "#F4511E", "#D84315", 
                 "#E57373", "#F44336", "#D32F2F", "#EC407A", "#D81B60", "#424242")

## Below will draw 3 charts

for(i in 2014:2016){
  
  png(filename=paste0("images/transport/all_transport_",i,".png"), width=2400, height=2400, res=200)
      #width=2400, height=2400, res=200)
  
  chord.df <- all.transport %>% filter(year==i) %>% select(-year)
  
  circos.clear()
  #par(mar=c(0,0,0,0), bg="#ffffff", mfrow = c(1,1))
  
  circos.par(cell.padding=c(0,0), 
             points.overflow.warning=FALSE, 
             canvas.xlim=c(-1.5,1.5),
             start.degree=90)
  
  chordDiagram(chord.df, 
               grid.col=pref.colour, 
               annotationTrack = "grid", 
               transparency = 0.2, 
               directional = 1,
               direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow",
               diffHeight  = -0.04, ## The difference of height btwn two roots. set to negative so that start root is longer.
               link.sort = TRUE, 
               link.largest.ontop = TRUE)
  
  
  # Add labels.
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), 2, si, facing = "clockwise", adj = c(0,0.1),
                niceFacing = TRUE, 
                cex = 0.8, 
                sector.index = si, 
                track.index = 1)
    circos.axis(sector.index = si, labels.cex = 0.5)
  }
  
  dev.off()  
}







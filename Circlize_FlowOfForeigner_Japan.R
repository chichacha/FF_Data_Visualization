
## Data Source
# http://www.mlit.go.jp/sogoseisaku/soukou/sogoseisaku_soukou_fr_000023.html
# http://www.mlit.go.jp/common/001211903.pdf

library(swatches)
library(rvest)
library(tidyverse)
library(circlize)
library(NipponMap)

### Some Colour Work Here.  I want at leat 48 different colours to display 47 prefecture + 1 unknown.

## using swatches package I can import "ASE" file! 
gColor<- read_ase(path="color_swatches/illustrator/Google/Material Palette.ase")

show_palette(gColor[grepl("(200|500)",names(gColor))])
show_palette(gColor[grepl("(Grey)", names(gColor))])
#my.gcol <-gColor[grepl("(800|500)",names(gColor))]

gColor.df <- tibble(
  name=gsub("(\\- Primary)","",names(gColor)),
  hex =gColor
)

gColor.df <- gColor.df %>% 
  mutate(color.group = trimws(str_extract(name,"[A-z ]*")), 
         color.value=as.numeric(str_extract(name,"[[:digit:]]+"))) %>%
  add_count(color.group)

unique(gColor.df$color.value)
gColor10.df <- gColor.df %>% filter(n==10) %>% mutate(color.group.f = fct_inorder(color.group)) %>% 
  filter(!color.group %in% c("Pink","Purple","Deep Purple","Red","Grey","Blue Grey","Brown")) %>%
  filter(color.value %in% c(200,400,600,800)) %>% arrange(color.group.f, -color.value)

## Use 1-47 Colours + 1 grey for Unknown!
#pref.colour <- c(gColor10.df$hex[1:47],"#757575") 
pref.colour <- c("#BA68C8", "#9C27B0", "#7B1FA2", "#7E57C2", "#5E35B1", "#4527A0", 
                 "#7986CB", "#3F51B5", "#303F9F", "#42A5F5", "#1E88E5", "#1565C0", 
                 "#4FC3F7", "#03A9F4", "#0288D1", "#26C6DA", "#00ACC1", "#00838F", 
                 "#4DB6AC", "#009688", "#00796B", "#66BB6A", "#43A047", "#2E7D32", 
                 "#AED581", "#8BC34A", "#689F38", "#D4E157", "#C0CA33", "#9E9D24", 
                 "#FFF176", "#FFEB3B", "#FBC02D", "#FFCA28", "#FFB300", "#FF8F00", 
                 "#FFB74D", "#FF9800", "#F57C00", "#FF7043", "#F4511E", "#D84315", 
                 "#E57373", "#F44336", "#D32F2F", "#EC407A", "#D81B60", "#424242")

## I need palette with 48 colours...
cairo_pdf(filename="JapanMap.pdf", width=9, height=9, family="Avenir")
JapanPrefMap(col=pref.colour[1:47]) 
#JapanPrefMap(col=wa.colour[47:1*8])
dev.off()

wa.colour <- read_html("https://www.colordic.org/colorscheme/") %>% 
  html_nodes(".well:nth-child(2) a") %>% 
  html_attr("style")

wa.colour <- wa.colour %>% str_remove("border-color:")
my.col <-wa.colour[1:48*9]


###  Japan Prefecture Data Preps
prefecture.table <- read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan") %>% 
  html_nodes(xpath= '//*[@id="mw-content-text"]/div/table[4]') %>% html_table()
prefecture.table <-prefecture.table[[1]]

## Arrange Table by ISO code so that it matches data
prefecture.table <- prefecture.table %>% arrange(ISO)
# prefecture.table$Prefecture
prefs.en <- c(prefecture.table$Prefecture,"Unknown")
prefs.en <- gsub("(Kochi|Oita)","",prefs.en)

prefecture.table$colour <- pref.colour[1:47]
## 
prefecture.table <- prefecture.table %>% group_by(Region) %>% 
  mutate(sub.rank=row_number(ISO)) %>% ungroup() %>% 
  mutate(Region.f = fct_inorder(Region), reg.rank=dense_rank(Region.f))

## What Colour Am I Using for Which Prefecture?
cairo_pdf(filename="JapanPrefectureColour.pdf", width=11, height=7, family="Osaka")
ggplot(prefecture.table, aes(x=reg.rank, y=sub.rank)) + 
  geom_point(size=35, alpha=0.9, aes(color=ISO)) + 
  scale_color_manual(values=pref.colour[1:47], guide="none") +
  geom_text(aes(label=paste0(Kanji,"\n",Prefecture)), color="black") + 
  theme_void() + 
  scale_y_reverse() +
  scale_x_discrete(position="top")
dev.off()

### Data Preps

sheet.data <- tibble(
  sheet.path = c(rep("2016_Nationality_OD.xlsx",10),rep("2016_Transport_OD.xlsx",9)),
  sheet.num = rep(seq(1,10),seq(1,9)),
  sheet.name = c(c("All Nationalities","Korea","Taiwan","Hong Kong","China",
                 "ASEAN","Europe","North America","Oceania","Other"),
                 c("All Transportation","Bus","Train","Taxi","Rental Car",
                   "Other Vehicles","Domestic Flight","Others Transportation","Unknown")),
  sheet.cat = c(rep("Nationality",10), rep("Transport",9))
)

## Run Below to generate PDFs!
for(i in 1:nrow(sheet.data)){
  sheet1 <-readxl::read_excel(path= sheet.data$sheet.path[i], 
                              sheet=sheet.data$sheet.num[i], skip=7, col_names=F)
  m1 <- sheet1[4:51,4:51]
  m1 <- m1 %>% transmute_all(as.numeric)
  m1 <- as.matrix(m1)
  
  colnames(m1) <- prefs.en
  rownames(m1) <- prefs.en
  
  #png(filename=paste0(sheet.data$sheet.cat[i],sheet.data$sheet.num[i],sheet.data$sheet.name[i],".png"), 
      #width=2400, height=2400, res=200)
  cairo_pdf(filename=paste0(sheet.data$sheet.cat[i],sheet.data$sheet.num[i],sheet.data$sheet.name[i],".pdf"), 
            width=9, height=9, family="Avenir")
  circos.clear()
  par(mar=c(0,0,0,0), bg="#ffffff", mfrow = c(1,1))
  
  circos.par(cell.padding=c(0,0), 
             points.overflow.warning=FALSE, canvas.xlim=c(-1.5,1.5),
             start.degree=90)
  
  chordDiagram(m1, grid.col=pref.colour, annotationTrack = "grid", transparency = 0.2, directional = 1)
  
  
  # Add labels.
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), 2, si, facing = "clockwise", adj = c(0,0.1),
                niceFacing = TRUE, cex = 0.5, sector.index = si, track.index = 1)
    circos.axis(sector.index = si, labels.cex = 0.2)
  }
  dev.off()
}




### If I were to do one by one

## I can change path for different sheet! 
my.filepath <- "2016_Nationality_OD.xlsx"
my.filename <- gsub("(.xlsx)","",my.filepath)
my.sheet <- 1 #1-9 sheets
sheet <-readxl::read_excel(path=my.filepath, sheet=my.sheet, skip=7, col_names=F)

m <- sheet[4:51,4:51]
m <- m %>% mutate_all(as.numeric)
m <- as.matrix(m)

colnames(m) <- prefs.en
rownames(m) <- prefs.en

## I can also convert this to df
m.df <- as.tibble(m)
m.df$origin <- prefs.en
m.df <- m.df %>% select(origin, everything())
m.df.long <- m.df %>% gather(key="destination", value="value", -origin)

m.df.long %>% count(origin, destination, wt=value, sort=T) %>% filter(origin!=destination)


circos.clear()
par(mar=c(0,0,0,0), bg="#ffffff", mfrow = c(1,1))
chordDiagram(m.df.long, grid.col=pref.colour, annotationTrack = c("grid","name"))

##  Now for the Fun!  Visualization of Data
#getwd()
#png(filename=paste0(my.filename,my.sheet,".png"), width=2400, height=2400, res=200)
cairo_pdf(filename=paste0(my.filename,my.sheet,".pdf"), width=10, height=10, family="Avenir")
circos.clear()
par(mar=c(0,0,0,0), bg="#ffffff", mfrow = c(1,1))

circos.par(cell.padding=c(0,0), 
          points.overflow.warning=FALSE, canvas.xlim=c(-1.5,1.5),
           start.degree=90)

chordDiagram(m, grid.col=pref.colour, annotationTrack = c("grid"), transparency = 0.2, 
             directional = 1, self.link = 2)

# Add labels.
for(si in get.all.sector.index()) {
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  circos.text(mean(xlim), 2, si, facing = "clockwise", adj = c(0,0.1),
              niceFacing = TRUE, cex = 0.6, sector.index = si, track.index = 1)
  circos.axis(sector.index = si, labels.cex = .2)
}
dev.off()

m.df.long %>% count(origin, wt=value, sort=T)
m.df.long %>% count(destination, wt=value, sort=T)

9351+8829

#read_excel(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)


## Import 2016 
ff_db_2016 <-readxl::read_xlsx(path="Data/2016_Basic_DB.xlsx", skip=2, col_names = F)
head(ff_db_2016)

ff_db_2016.df <-ff_db_2016[c(-1,-2),]
names(ff_db_2016.df) <- c("PortOfExitCd","PortOfExit","NationalityCd",
                          "Nationality","TripPurposeCd","TripPurpose",
                          "OriginType","OriginCd","OriginName",
                          "DestType","DestCd","DestName", 
                          "TransportCd","Transport",
                          "Q1Volume","Q2Volume","Q3Volume","Q4Volume","YearVolume")

ff_db_2016.df <- ff_db_2016.df %>% mutate_at(vars(Q1Volume:YearVolume),as.numeric)
ff_db_2016.df <- ff_db_2016.df %>% mutate_at(vars(NationalityCd,TripPurposeCd,OriginCd, DestCd, TransportCd),as.numeric)

ff_db_2016.df %>% count(NationalityCd, Nationality, wt=YearVolume)  %>% arrange(NationalityCd) %>% print(n=30)

## 2016 Who came to Japan?
ff_db_2016.df <-ff_db_2016.df %>% mutate(Nationality.f = fct_reorder(Nationality,YearVolume,sum, na.rm=T),
                         Transport.f = fct_reorder(Transport, YearVolume, sum, na.rm=T)) 
ff_db_2016.df %>% 
  count(Nationality.f,TripPurpose,wt=YearVolume, sort=T) %>%
  ggplot(aes(x=Nationality.f, y=n, fill=TripPurpose)) + 
  geom_bar(stat="identity", position="stack") + ## change to fill or stack
  coord_flip() +
  theme_ipsum(base_family = "Hiragino Sans W6") + 
  scale_y_comma() +
  scale_fill_gdocs()

ff_db_2016.df %>% 
  count(Nationality.f,TripPurpose,wt=YearVolume, sort=T) %>%
  ggplot(aes(x=Nationality.f, y=n, fill=TripPurpose)) + 
  geom_bar(stat="identity", position="fill") + ## change to fill or stack
  coord_flip() +
  theme_ipsum(base_family = "Hiragino Sans W6") + 
  scale_y_comma() +
  scale_fill_gdocs()

update_geom_font_defaults(family = "Hiragino Sans W6")
ff_db_2016.df %>% count(OriginType, wt=YearVolume, sort=T)
ff_db_2016.df %>% count(DestType, wt=YearVolume, sort=T)
ff_db_2016.df %>% count(PortOfExit, wt=YearVolume, sort=T)
ff_db_2016.df %>% count(TripPurpose,Transport.f, wt=YearVolume, sort=T) %>% 
  ggplot(aes(x=TripPurpose, y=n,fill=Transport.f) ) + 
  geom_bar(stat="identity",position="fill") + 
  theme_ipsum(base_family="Hiragino Sans W6") +
  coord_flip() + scale_fill_gdocs()

ff_db_2016.df %>% filter(grepl("01",DestType)) %>% 
  count(DestCd,DestType, DestName, wt=YearVolume, sort=T)


tmp <- tibble(
  n1 = transpose(ff_db_2016[c(1),]) %>% unlist,
  n2 = transpose(ff_db_2016[c(2),]) %>% unlist
)

tmp <- tmp %>% fill(n1) %>% mutate(col.name=str_c(n1,n2))

m.df.long %>% count(origin, destination, sort=T, wt=value) %>% filter(origin=="Chiba")

ff_db_2016.df %>% filter(DestType=="01訪問地"&OriginType=="01訪問地") %>% 
  count(OriginName,DestName,wt=YearVolume, sort=T)


test <-gColor.df %>% filter(!grepl(" A", color.group) & n==10 &
                            !grepl("(Grey|Brown)", color.group) & color.value>200 & color.value<900) %>%
  mutate(color.group.f = fct_inorder(color.group)) %>%
  mutate(color.group.f = fct_relevel(color.group.f, c("Red","Pink"), after=Inf)) %>%
  arrange(color.group.f, color.value) %>% mutate(color.group.eo = dense_rank(color.group.f) %% 2) %>%
  group_by(color.group.f) %>% mutate(color.value.eo = row_number(color.value) %% 2)  %>% 
  filter(color.value.eo==color.group.eo) %>% 
  arrange(color.group.f) 

test$hex %>% clipr::write_clip()

show_palette(test$hex)

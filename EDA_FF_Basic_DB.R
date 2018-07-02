## Trying to understand Basic DB set is

library(readxl)
library(tidyverse)
library(countrycode)
library(googleLanguageR)
library(scales)
library(ggthemes)




DB_2016 <- read_excel("Data/2016_Basic_DB.xlsx", skip = 3)
DB_2014 <- read_excel("Data/2014_Basic_DB.xlsx", skip = 2)
DB_2015 <- read_excel("Data/2015_Basic_DB.xlsx", skip = 3)
## Show 10 recordes randomly!
DB_2016 %>% sample_n(10)
names(DB_2016)
col.names <- c("airport_cd","airport","nationality_cd","nationality",
               "purpose_cd","purpose",
               "category","pref_cd","pref_name",
               "category2","pref_cd2","pref_name2",
               "tran_cd","tran",
               "q1","q2","q3","q4","full_year")

names(DB_2016) <- col.names
names(DB_2014) <- col.names
names(DB_2015) <- col.names
#
#trans.lookup <- DB_2016 %>% 
#  select(purpose, tran, nationality) %>% 
#  gather(key="key", value="japanese") %>%
#  distinct()


#Only run when needed
#trans.lookup <- trans.lookup %>%
#  mutate(english = map(japanese, gl_translate, target = "en", source ="ja"))

trans.lookup <- trans.lookup %>% mutate(translation = str_to_title(map_chr(english,1)))

## just want to tweak bit
trans.lookup <- trans.lookup %>% 
  mutate(translation = case_when(translation %in% c("Usa","Uk") ~ str_to_upper(translation),
                                 translation %in% c("Visit By Family / Acquaintance") ~ "Visiting Family/Friends",
                                 translation %in% c("Domestic Plane Airplane") ~ "Domestic Flights",
                                 TRUE ~ translation))


                                
DB_Comb <- bind_rows("2014" = DB_2014,"2015" = DB_2015 , "2016" = DB_2016, .id = "year") %>%
  filter(category == category2) %>%
  mutate(year = as.integer(year))

DB_Comb <- DB_Comb %>% 
  left_join(trans.lookup %>% filter(key == "nationality") %>% select(japanese, nationality_en=translation), 
            by=c("nationality"="japanese")) %>%
  left_join(trans.lookup %>% filter(key == "purpose") %>% select(japanese, purpose_en=translation), 
            by=c("purpose"="japanese")) %>%
  left_join(trans.lookup %>% filter(key == "tran") %>% select(japanese, tran_en=translation), 
          by=c("tran"="japanese"))


## Why Visit Japan?
DB_Comb %>% 
  count(year, purpose_en, sort=T, wt=full_year)

## Who Visit Japan
total.by.nationality <-DB_Comb %>%
  count(year,nationality_en, sort=T, wt=full_year) %>%
  group_by(nationality_en) %>% 
  arrange(year) %>%
  mutate(yoy = (n - lag(n))/lag(n))

## Who comes to visit Japan and what purpose
DB_Comb %>% 
  count(year ,purpose_en,nationality_en, sort=T, wt=full_year) %>%
  ggplot(aes(x=fct_reorder(nationality_en,n,sum), y=n, fill=fct_reorder(purpose_en,n,sum,.desc=T))) +
  geom_col(position="stack") +
  theme_ipsum_rc() +
  coord_flip() +
  scale_y_continuous(label=unit_format(unit = "M", scale = 1e-6, digits=1)) +
  scale_fill_gdocs(name="Purpose of Visit") +
  facet_wrap(~year, scales="free") +
  labs(title="Where are they coming to visit Japan from?", 
       subtitle="For what purpose?", x="Nationality", y="Number of Visitors") +
  expand_limits(y=15000000) +
  geom_text(data=total.by.nationality, 
            aes(label=paste0(round(n/1000000,2),"M ",
                             ifelse(year==2014,"",paste0("(",percent(yoy),")"))), 
                fill=NULL),
            family = "Roboto Condensed", hjust=0) +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1))


## Usage of Transportation by Nationality

DB_Comb %>% 
  count(year ,purpose_en,nationality_en, sort=T, wt=full_year) %>%
  ggplot(aes(x=fct_reorder(nationality_en,n,sum), y=n, fill=fct_reorder(purpose_en,n,sum,.desc=T))) +
  geom_col(position="fill") +
  theme_ipsum_rc() +
  coord_flip() +
  scale_y_continuous(label=percent) +
  scale_fill_gdocs(name="Purpose of Visit") +
  facet_wrap(~year, scales="free") +
  labs(title="Purpose of Visit by Nationality", 
       subtitle="Is there differences in purpose of visit by nationality", x="Nationality", y="") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1))

DB_Comb %>% 
  count(year ,tran_en,purpose_en, sort=T, wt=full_year) %>%
  ggplot(aes(x=fct_reorder(purpose_en,n,sum), y=n, fill=fct_reorder(tran_en,n,sum,.desc=T))) +
  geom_col(position="stack") +
  theme_ipsum_rc() +
  coord_flip() +
  scale_y_continuous(label=comma) +
  scale_fill_gdocs(name="Transportation Method") +
  facet_wrap(~year, scales="free_y") +
  labs(title="Reason to Visit Japan", 
       subtitle="Coloured by Transporation Used", x="Reason for Visiting Japan", y="") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1))





DB_Comb %>% 
  count(year ,tran_en,nationality_en, sort=T, wt=full_year) %>%
  ggplot(aes(x=fct_reorder(nationality_en,n,sum), y=n, fill=fct_reorder(tran_en,n,sum,.desc=T))) +
  geom_col(position="fill") +
  theme_ipsum_rc() +
  coord_flip() +
  scale_y_continuous(label=percent) +
  scale_fill_gdocs(name="Transportation Method") +
  facet_wrap(~year, scales="free") +
  labs(title="Transportation Usage by Nationality", 
       subtitle="What Transportation Gets Used", x="Nationality", y="") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1))


DB_Comb %>% 
  count(year ,tran_en,purpose_en, sort=T, wt=full_year) %>%
  ggplot(aes(x=fct_reorder(purpose_en,n,sum), y=n, fill=fct_reorder(tran_en,n,sum,.desc=T))) +
  geom_col(position="stack") +
  theme_ipsum_rc() +
  coord_flip() +
  scale_y_continuous(label=percent) +
  scale_fill_gdocs(name="Transportation Method") +
  facet_wrap(~year, scales="free") +
  labs(title="Transportation Usage by Nationality", 
       subtitle="What Transportation Gets Used", x="Nationality", y="") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1))

DB_Comb %>% select(tran_cd, tran_en, tran) %>% distinct()

###  Maybe Now only look at visitor who is in Japan for Tourism and uses Railway to travel.
DB_Comb %>% filter(tran_en == "Railway" & purpose_cd == 1) %>%
  count(year ,pref_name2, wt=full_year, sort=T)


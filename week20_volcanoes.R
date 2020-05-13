# libraries
library(readxl)
library(tidyverse)
library(ggExtra)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggmap)

# set plot theme
#theme_set(ggsidekick::theme_sleek())

# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 20)

# cleaning
eruptions <- tuesdata$eruptions #%>% 
  #janitor::clean_names() %>% 
  #select(-contains("modifier"), -contains("uncertainty"))
events <- tuesdata$events #%>% 
  #janitor::clean_names() %>% 
  #select(-contains("modifier"), -contains("uncertainty"))
volcanoes <- tuesdata$volcano #%>% 
  #janitor::clean_names()
treerings <- tuesdata$tree_rings #%>% 
  #janitor::clean_names()

# explore
summary(eruptions)
summary(events)
summary(volcanoes)
summary(treerings)

# what do i want to explore?
# most destructive eruptions with population within 5km

unique(eruptions$eruption_category)
unique(events$event_type)

v.data <- merge(volcanoes,eruptions,by="volcano_number")
summary(v.data)
v.data<-v.data[!duplicated(v.data),]

v.data$last_eruption_year <- as.numeric(v.data$last_eruption_year)
destruct <- v.data %>% 
  filter(eruption_category=="Confirmed Eruption" & vei>=5 & population_within_5_km>=1000 &
           last_eruption_year>1980)

destruct %>% ggplot()+
  geom_jitter(aes(last_eruption_year,vei,size=vei,color=vei))+
  #scale_fill_gradient2(low='yellow',mid='orange',high='red',aesthetics='fill')+
  #ggsidekick::theme_sleek()+
  xlab("Date of last eruption")+
  ylab("VEI (Explosivity Index)")+
  theme(legend.position='none')+
  theme(panel.background = element_rect(fill='black'))

# load world map
#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)
#ggplot(data = world) +
  #geom_sf()
#ggplot(data = world) +
  #geom_sf(colour='grey',fill='#008080') +
  #xlab("Longitude") + ylab("Latitude") +
  #ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

#library("ggspatial")
#world %>% filter(name=="New Zealand") %>% 
  #ggplot() +
  #geom_sf(colour='grey',fill='#008080') +
  #xlab("Longitude") + ylab("Latitude")+
  #annotation_scale(location = "bl", width_hint = 0.5) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
                         #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         #style = north_arrow_fancy_orienteering) +
  #coord_sf(xlim = c(166, 180), ylim = c(-55,-30))

# use ggmap instead
NZ<- c(lon = 174, lat = -40)
taupo <- c(long=176,lat=-38.6)
NZmap <- get_map(taupo, zoom=7, scale=1, maptype = "watercolor", source = "stamen")
ggmap(NZmap)
# largest NZ eruptions
unique(v.data$subregion)
NZ.volc <- v.data %>% filter(subregion=="New Zealand" & last_eruption_year>2000) %>% 
  group_by(volcano_name.x) %>% 
  summarise(
    lat=mean(latitude.x),
    long=mean(longitude.x),
    max_VEI=max(vei,na.rm=TRUE))

NZ.volc <- NZ.volc %>% filter(!volcano_name.x=="Kaikohe-Bay of Islands")
colnames(NZ.volc)[1]<-"Volcano"

ggmap(NZmap,
      base_layer = ggplot(NZ.volc,aes(x=long,y=lat)))+
  geom_point(aes(size=max_VEI),color="darkred",shape=2,stroke=2)+
  geom_text(aes(label=Volcano,hjust=1,vjust=1))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

# from https://github.com/tacookson/tidy-tuesday/blob/master/volcano-eruptions.Rmd
# {r setup-and-import}
library(tidyverse)
library(lubridate)
library(gganimate)
library(extrafont)
library(ggtext)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# {r eruptions-animation-by-year}
p <- eruptions %>%
  # Only non-discredited eruptions since 1970
  # Only ones where we know the VEI (since VEI is mapped to size of point)
  filter(start_year >= 1970,
         eruption_category != "Discredited Eruption",
         !is.na(vei)) %>%
  # Convert date fields into decimal year (e.g., Jun 2016 becomes 2016.5)
  mutate(start_date = decimal_date(ymd(
    paste(
      year = start_year,
      month = coalesce(pmax(start_month, 1), 1),
      day = coalesce(pmax(start_day, 1), 1),
      sep = "-"
    )
  ))) %>%
  ggplot(aes(longitude, latitude, size = vei, group = start_year)) +
  borders(colour = "grey90", fill = "grey90") +
  geom_point(colour = "#e04324",
             shape = 16,
             alpha = 0.3) +
  scale_radius(range = c(0.1, 9)) +
  transition_time(start_year, range = c(1969, 2020)) +
  labs(title = "KABOOM! The Past 50 Years of Volcanic Activity",
       subtitle = "Volcanic <span style = 'color:#e04324'>**eruptions**</span> from 1970 to {pmax(round(frame_time, 0), 1970)}",
       caption = "Source: Nature | Visualization: @alexcookson") +
  enter_grow() +
  shadow_mark() +
  ggthemes::theme_map() +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Bahnschrift", size = 22),
    plot.subtitle = element_text(family = "Bahnschrift", size = 18),
    plot.caption = element_text(family = "Bahnschrift", size = 12)
  )
anim <- animate(p, fps = 10, nframes = 10 * 30, start_pause = 10, end_pause = 25, width = 800, height = 500)
anim_save("volcano-eruptions-1970-2020-short.gif", anim)

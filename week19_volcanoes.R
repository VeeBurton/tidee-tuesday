# libraries
library(readxl)
library(tidyverse)
library(ggExtra)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# set plot theme
theme_set(ggsidekick::theme_sleek())

# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 20)

# cleaning
eruptions <- tuesdata$eruptions %>% 
  janitor::clean_names() %>% 
  select(-contains("modifier"), -contains("uncertainty"))
events <- tuesdata$events %>% 
  janitor::clean_names() %>% 
  select(-contains("modifier"), -contains("uncertainty"))
volcanoes <- tuesdata$volcano %>% 
  janitor::clean_names()
treerings <- tuesdata$tree_rings %>% 
  janitor::clean_names()

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
v.data[!duplicated(v.data),]

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
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()
ggplot(data = world) +
  geom_sf(colour='grey',fill='#008080') +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

library("ggspatial")
world %>% filter(name=="New Zealand") %>% 
  ggplot() +
  geom_sf(colour='grey',fill='#008080') +
  xlab("Longitude") + ylab("Latitude")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(166, 180), ylim = c(-55,-30))

# largest NZ eruptions
unique(v.data$subregion)
NZ <- v.data %>% 
  filter(subregion=="New Zealand") %>% 
  ggplot()+
  geom_jitter(aes(x=longitude.x,y=latitude.x,size=vei))

NZ + map

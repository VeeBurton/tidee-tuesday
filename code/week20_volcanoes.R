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

# from https://github.com/jpwrobinson/tidytuesday/blob/master/volcano_tidytuesday_120520.R
library(tidyverse)
library(devtools)
install_github('jpwrobinson/funk')
library(funk)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

head(volcano) %>% data.frame()
head(eruptions) %>% data.frame()
head(events) %>% data.frame()


obs<-eruptions %>% filter(evidence_method_dating == 'Historical Observations' & start_year > 1800 & volcano_name !='Unknown Source') %>%
  select(volcano_name, volcano_number, vei, start_year, end_year, latitude, longitude) %>%
  group_by(volcano_name) %>% mutate(freq = length(volcano_name), max.vei = max(vei, na.rm=TRUE)) %>%
  ungroup() 

obs<-left_join(obs, volcano) %>%
  mutate(volcano_name = fct_reorder(volcano_name, latitude))
obs$region<-factor(obs$region)

plotter<-obs %>% filter(vei > 3 & elevation > 0) %>% mutate(long.lab=round(longitude, 0), population_within_10_km=log10(population_within_10_km+1)) 

left<-ggplot(plotter, aes(volcano_name, start_year, fill = population_within_10_km, size=vei)) +
  geom_point(alpha=0.9, shape=21,colour='black',stroke=1.2) + 
  coord_flip() +
  labs(x='', y='') +
  scale_size_area(breaks=c(4,5,6), trans='exp', max_size=30) +
  scale_fill_gradient(na.value='white', low='#ffffcc', high='#de2d26') +
  scale_y_continuous(breaks=seq(1800, 2020, 20)) +
  scale_x_discrete(expand=c(0.025,0.025)) +
  theme_classic() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_line(colour='grey90'), 
        axis.text = element_text(size=9, colour='white'),
        panel.border = element_blank(),
        title = element_text(colour='white', size=12, face='bold', hjust=0),
        plot.subtitle = element_text(colour='white', size=9, face='bold', hjust=0),
        plot.background = element_rect(color='black',fill = "black"),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        plot.margin=unit(c(0.5, 0.2, 0, 0), 'cm'))

plotter2<-plotter %>% group_by(long.lab, volcano_name, region) %>% summarise(freq = unique(freq))
lab<-plotter2$long.lab
lab<-ifelse(lab > 0, paste0(lab, 'N'), paste0(-lab, 'S'))

right<-ggplot(plotter2) +
  geom_bar(aes(volcano_name, freq, fill=region), stat='identity') +
  geom_text(aes(x=volcano_name, y=freq, label = region, col=region), size=2,hjust = -0.1) +
  coord_flip() + 
  labs(y = 'Total number of eruptions since 1800', x = '') +
  theme_classic() + 
  theme(
    axis.text.y = element_text(size=9, colour='white', hjust=0.5),
    axis.text.x = element_text(size=9, colour='white'),
    axis.ticks=element_blank(), 
    axis.line.x = element_line(color='white'),
    legend.position = 'none',
    plot.background = element_rect(color='black', fill = "black"),
    panel.border = element_blank(),
    plot.subtitle = element_text(colour='white', size=9, face='bold', hjust=0),
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    plot.margin=unit(c(0.5, 1, 0.2, -0.5), 'cm')) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 100, 25)) +
  scale_x_discrete(expand=c(0.025,0.025), label=lab) 
right


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(ggplot(plotter, aes(volcano_name, start_year, fill = population_within_10_km, size=vei)) +
                     geom_point(alpha=0.9, shape=21,colour='grey90',stroke=1.2) +
                     scale_size_area(breaks=c(4,5,6), trans='exp', max_size=30, name='Volcanic Explosivity Index') +
                     scale_fill_gradient(na.value='white', low='#ffffcc', high='#de2d26', breaks=seq(0, 6, 2), labels=c(0, 100, '10K', '1 million'), name='Population < 10 km') +
                     theme(plot.margin=unit(c(0, 0, 0, 0), 'cm'),
                           legend.position = 'bottom',
                           legend.box.background = element_rect(color='black', fill = "black"),
                           plot.background = element_rect(color='black', fill = "black"),
                           panel.background = element_rect(color='black', fill = "black"),
                           panel.border=element_blank(),
                           legend.background = element_rect(fill='black'),
                           legend.key = element_rect(fill='black'),
                           legend.text = element_text(colour='white'),
                           legend.title = element_text(colour='white')))

gleg2<-ggplot() + theme_void() + 
  annotation_custom(grob = legend, 
                    xmin = 0.5, xmax = 0.6, ymin = 0.6, ymax = 0.7)  +
  theme(plot.background = element_rect(color='black', fill = "black"),
        panel.background = element_rect(color='black', fill = "black"),
        legend.key.size = unit(2,"line"),
        panel.border=element_blank(),
        plot.margin=unit(c(0, -2, -2.3, -2), 'cm'))
gleg2

pdf(file = 'volcano.pdf', height=11, width=10)
t<-cowplot::plot_grid(left, right, rel_widths = c(1, 0.5), nrow=1,  label_size=15, 
                      labels=c('Catastrophic eruptions since 1800', 'Total number of eruptions'), hjust=-0.25, label_colour='white')
cowplot::plot_grid(gleg2, t, nrow=2, rel_heights=c(0.1,1))
dev.off()

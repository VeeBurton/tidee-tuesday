
# date: 13/05/20
# Datacamp course "Introduction to spatial data" - useful libraries and code snippets

library(ggmap)
# API from google, store permanently in .Renviron file
register_google(key = "AIzaSyB_Sft1bVREQZ8T7BsTUzyg44L_MMgqM6I", write=TRUE)

# find locations googling "location coordinates"
edinburgh <- c(lon = -3.1, lat = 55.9)
# Get map at zoom level 5: map_5
map <- get_map(edinburgh, zoom = 10, scale = 1)
ggmap(map)

# to add additional (point) data, swap out ggplot for a ggmap call e.g.

ggmap(map)+
  geom_point(aes(x,y,size=z), data=df)

# nicer styles
stamen.burgh <- get_map(edinburgh, zoom=10,scale=1,
                        maptype = "watercolor",
                        source = "stamen")
ggmap(stamen.burgh)

# baselayer argument in ggmap allows faceting
ggmap(stamen.burgh,
      base_layer=ggplot(df, aes(x,y)))+
  geom_point()+
  facet_wrap(~z)

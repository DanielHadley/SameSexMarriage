#### Load packages and data ####
library(XML)
library(ggplot2)
library(maps)
library(reshape2)
library(plyr)
library(stringr)
# Maping tools
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("ggmap")
library(scales) # for changing from scientific notation



# load data, which was scraped from wikipedia on Nov 16th, 2014
PolsByState <- read.csv("./data/PolsByState.csv")
PollsByState <- read.csv("./data/PollsByState.csv")


#### Map it ####

##  First the lower 48 
# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
#load us map data
# I use map_data instead of my shapefile b/c I was having a bounding problem
# which I fixed through this hack:
#: http://www.exegetic.biz/blog/2013/12/contour-and-density-layers-with-ggmap/

all_states <- map_data("state")


# Prep for the map
# I create a new df that is similar to all_states because merging ruins the shapefiles from all_states 
State <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,all_states$region, perl=TRUE)
State <- as.data.frame(State)
all_statesTwo <- merge(State, PolsByState, by="State", all.x=T)
all_states$PercOfPolsSupp <- all_statesTwo$PercOfPolsSupp 
all_states$PercOfPolsOpp <- all_statesTwo$PercOfPolsOpp


# Map
map <- get_map(location = "USA", zoom=4, maptype="roadmap", color = "bw")
ggmap(map)

# Now the full 50 for the boxes with Hawaii and Alaska
us50_shp <- readShapePoly("./shapefiles/FiftyStates/cb_2013_us_state_20m.shp")
us50_df <- as.data.frame(us50_shp)

us50_points <- sp2tmap(us50_shp)
names(us50_points) <- c("id", "x", "y")
us50_df$DRAWSEQ <- 1:52
us50 <- merge(x = us50_df, y = us50_points, by.x = "DRAWSEQ", by.y = "id")

# prepare an identical dataframe to loan columns to the shapefile df
us50Two <- merge(us50, PolsByState, by.x="NAME", by.y="State", all.x=T)
us50Two <- us50Two[order(us50Two$DRAWSEQ) , ]
us50$PercOfPolsSupp <- us50Two$PercOfPolsSupp 
us50$PercOfPolsOpp <- us50Two$PercOfPolsOpp

# HI & AK Map
Hawaiimap <- get_map(location = "Hawaii, USA", zoom=8, maptype="roadmap", color = "bw")
ggmap(Hawaiimap)

Alaskamap <- get_map(location = "Alaska, USA", zoom=4, maptype="roadmap", color = "bw")
ggmap(Alaskamap)



# Opponents 48
ggmap(map,extent = "normal", maprange=FALSE) +
  geom_polygon(data=all_states, aes(x=long, y=lat, group=group, fill=all_states$PercOfPolsOpp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Opposed to Gay Marriage, 2014")

ggsave(paste("./plots/GMapOpposed.png"), dpi=300, width=6, height=5)

# Opponents HI
ggmap(Hawaiimap,extent = "normal", maprange=T) +
  geom_polygon(data=us50, aes(x=x, y=y, group=DRAWSEQ, fill=us50$PercOfPolsOpp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Opposed to Gay Marriage, 2014")

ggsave(paste("./plots/GMapOpposedHI.png"), dpi=300, width=6, height=5)

# Opponents AK
ggmap(Alaskamap,extent = "normal", maprange=T) +
  geom_polygon(data=us50, aes(x=x, y=y, group=DRAWSEQ, fill=us50$PercOfPolsOpp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Opposed to Gay Marriage, 2014")

ggsave(paste("./plots/GMapOpposedAK.png"), dpi=300, width=6, height=5)



# Support 48
ggmap(map,extent = "normal", maprange=FALSE) +
  geom_polygon(data=all_states, aes(x=long, y=lat, group=group, fill=all_states$PercOfPolsSupp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Who Support Gay Marriage, 2014")

ggsave(paste("./plots/GMapSupport.png"), dpi=300, width=6, height=5)

# Support HI
ggmap(Hawaiimap,extent = "normal", maprange=T) +
  geom_polygon(data=us50, aes(x=x, y=y, group=DRAWSEQ, fill=us50$PercOfPolsSupp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Who Support Gay Marriage, 2014")

ggsave(paste("./plots/GMapSupportHI.png"), dpi=300, width=6, height=5)

# Support AK
ggmap(Alaskamap,extent = "normal", maprange=T) +
  geom_polygon(data=us50, aes(x=x, y=y, group=DRAWSEQ, fill=us50$PercOfPolsSupp), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("% Of Politicians Who Support Gay Marriage, 2014")

ggsave(paste("./plots/GMapSupportAK.png"), dpi=300, width=6, height=5)

# Politicians For and Against Gay Marriage
# Created by Daniel Hadley to analyze the support for gay marriage
# Nov, 2014
# setwd("/Users/dphnrome/Documents/Git/SameSexMarriage/")
setwd("C:/Users/dhadley/Documents/GitHub/SameSexMarriage")


#### Load packages and data ####

# Warning: installing my modified version seems to override the official package
# library('devtools')
# install_github(repo='choroplethrModified', username ="DanielHadley")

# load data, which was scraped from wikipedia on Nov 16th, 2014
PolsByState <- read.csv("./data/PolsByState.csv")

library(choroplethr)
library(choroplethrMaps)
library(ggplot2)


# Opposed
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsOpp * 100

state_choropleth(PolsByState,
                 title   = "Politicians Who Oppose Gay Marriage",
                 legend  = "% Opposed",
                 buckets = 1)

ggsave(paste("./plots/Map1.png"), dpi=300, width=7, height=4)


# Opposed
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsOpp * 100


choro = StateChoropleth$new(PolsByState)
choro$title = "Politicians Opposed to Gay Marriage"
choro$legend  = "% Opposed"
# choro$buckts = 1
choro$ggplot_scale = scale_fill_brewer(name="% Opposed", palette = "YlGnBu") 
choro$render()
ggsave(paste("./plots/Map2.png"), dpi=300, width=8, height=4)

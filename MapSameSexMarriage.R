# Politicians For and Against Gay Marriage
# Created by Daniel Hadley to analyze the support for gay marriage
# Nov, 2014
# setwd("/Users/dphnrome/Documents/Git/SameSexMarriage/")
setwd("C:/Users/dhadley/Documents/GitHub/SameSexMarriage")


#### Load packages and data ####

# Warning: installing my modified version seems to override the official package
library('devtools')
install_github(repo='choroplethrModified', username ="DanielHadley")

# load data, which was scraped from wikipedia on Nov 16th, 2014
PolsByState <- read.csv("./data/PolsByState.csv")
PollsByState <- read.csv("./data/PollsByState.csv")


library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library("gridExtra")


# Politician Support
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsSupp * 100

state_choropleth(PolsByState,
                 title   = "Politicians Who Support Gay Marriage",
                 legend  = "% Support",
                 buckets = 1)

ggsave(paste("./plots/Map1.png"), dpi=300, width=7, height=4)


# Public Support  
PollsByState$region <- tolower(PollsByState$state)
PolsByState$value <- PollsByState$support 

state_choropleth(PolsByState,
                 title   = "Public Support for Gay Marriage",
                 legend  = "% Support",
                 buckets = 1)

ggsave(paste("./plots/Map2.png"), dpi=300, width=7, height=4)



### put them together 

# Politician Support
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsSupp * 100

p1 = state_choropleth(PolsByState,
                      title   = "Politicians Who Support Gay Marriage",
                      legend  = "% Support",
                      buckets = 1)



# Public Support  
PollsByState$region <- tolower(PollsByState$state)
PolsByState$value <- PollsByState$support 

p2 = state_choropleth(PolsByState,
                      title   = "Public Support for Gay Marriage",
                      legend  = "% Support",
                      buckets = 1)

g = arrangeGrob(p1, p2)
ggsave(paste("./plots/Map3.png"), g, dpi=300, width=7, height=8)


# Public Support + No op  
PollsByState$region <- tolower(PollsByState$state)
PolsByState$value <- PollsByState$support + PollsByState$X..no.opinion 

state_choropleth(PolsByState,
                 title   = "Public Support for Gay Marriage",
                 legend  = "% Support OR no opinion",
                 buckets = 1)

ggsave(paste("./plots/Map4.png"), dpi=300, width=7, height=4)




# Opposed
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsOpp * 100

state_choropleth(PolsByState,
                 title   = "Politicians Who Oppose Gay Marriage",
                 legend  = "% Opposed",
                 buckets = 1)

ggsave(paste("./plots/Map5.png"), dpi=300, width=7, height=4)































# Opposed
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$PercOfPolsOpp * 100


choro = StateChoropleth$new(PolsByState)
choro$title = "Politicians Opposed to Gay Marriage"
choro$legend  = "% Opposed"
# choro$buckts = 1
choro$ggplot_scale = scale_fill_brewer(name="% Opposed", palette = "YlGnBu") 
choro$render()
ggsave(paste("./plots/Map5.png"), dpi=300, width=8, height=4)








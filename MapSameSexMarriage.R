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
library(gridExtra)


# Politician Support
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- round(PolsByState$PercOfPolsSupp * 100)

state_choropleth(PolsByState,
                 title   = "Politicians Who Support Gay Marriage",
                 legend  = "% Support")

ggsave(paste("./plots/Map1.png"), dpi=300, width=7, height=4)


# Public Support  
PollsByState$region <- tolower(PollsByState$state)
PolsByState$value <- PollsByState$support 

state_choropleth(PolsByState,
                 title   = "Public Support for Gay Marriage",
                 legend  = "% Support")

ggsave(paste("./plots/Map2.png"), dpi=300, width=7, height=4)



# Opposed
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- round(PolsByState$PercOfPolsOpp * 100)

state_choropleth(PolsByState,
                 title   = "Politicians Who Oppose Gay Marriage",
                 legend  = "% Opposed")

ggsave(paste("./plots/Map3.png"), dpi=300, width=7, height=4)



# Public Opposition  
PollsByState$region <- tolower(PollsByState$state)
PolsByState$value <- PollsByState$X..opposition 

state_choropleth(PolsByState,
                 title   = "Public Opposition to Gay Marriage",
                 legend  = "% Opposed")

ggsave(paste("./plots/Map4.png"), dpi=300, width=7, height=4)



# Gov Support
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$GovsSupport

state_choropleth(PolsByState,
                 title   = "Governors Who Support Gay Marriage",
                 legend  = "Support")

ggsave(paste("./plots/Map5.png"), dpi=300, width=7, height=4)



# Gov Oppose
PolsByState$region <- tolower(PolsByState$State)
PolsByState$value <- PolsByState$GovsOppose

state_choropleth(PolsByState,
                 title   = "Governors Who Oppose Gay Marriage",
                 legend  = "Oppose")

ggsave(paste("./plots/Map6.png"), dpi=300, width=7, height=4)

































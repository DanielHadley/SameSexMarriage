# Politicians For and Against Gay Marriage
# Created by Daniel Hadley to analyze the support for gay marriage
# Nov, 2014
setwd("/Users/dphnrome/Documents/Git/SameSexMarriage/")
# setwd("C:/Users/dhadley/Documents/GitHub/SameSexMarriage")


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


# load data, which was scraped from wikipedia on Nov 16th, 2014
polls <- read.csv("./data/PollingData.csv")
supporters <- read.csv("./data//PoliticianData.csv")
opponents <- read.csv("./data//PoliticianOpponentData.csv")
# congress data: https://www.govtrack.us/data/congress-legislators/
congress <- read.csv("./data/legislators-current.csv")
abbreviations <- read.csv("./data//us_states.csv", header=F)
congress <- merge(congress, abbreviations, by.x="state", by.y="V3", all.x=T)
remove(abbreviations)

# States data from https://www.census.gov/popest/data/state/asrh/2013/files/SCPRC-EST2013-18+POP-RES.csv
# Population for 18+
# stopped using this because population is not a useful denominator
# Now just used to make the list for wikipedia mining
states <- read.csv("./data/States.csv")


#### Scrape data from Wikipedia ####
# # Only do this if you want to update everything
# # Otherwise load data above
# 
# # List of supporters:
# 
# # Read and parse HTML file
# doc.html = htmlTreeParse('http://en.wikipedia.org/wiki/List_of_supporters_of_same-sex_marriage_in_the_United_States#Elected_officials',
#                          useInternal = TRUE)
# 
# # Extract all the paragraphs (HTML tag is p, starting at
# # the root of the document). Unlist flattens the list to
# # create a character vector.
# doc.text = unlist(xpathApply(doc.html, '//li', xmlValue))
# 
# # make a df
# d <- as.data.frame(doc.text)
# 
# write.csv(d, "./data/PoliticianData.csv")
# 
# 
# # Polling data to compare to politicians:
# 
# # Read and parse HTML file
# theurl <- 'http://en.wikipedia.org/wiki/Public_opinion_of_same-sex_marriage_in_the_United_States#By_state'
# table <-  readHTMLTable(theurl, header=T, which=27,stringsAsFactors=F)
# 
# colnames(table) <- c("State", "% support\n(at 95% confidence level)", "support","% opposition", 
#                      "% opposition.y", "% no opinion", "% no opinion.y", "Date of poll", "Sample size")
# 
# write.csv(table, "./data/PollingData.csv")

# List of opponents:

# # Read and parse HTML file
# doc.html = htmlTreeParse('http://en.wikipedia.org/wiki/List_of_opponents_of_same-sex_marriage_in_the_United_States#Elected_officials',
#                          useInternal = TRUE)
# 
# # Extract all the paragraphs (HTML tag is p, starting at
# # the root of the document). Unlist flattens the list to
# # create a character vector.
# doc.text = unlist(xpathApply(doc.html, '//li', xmlValue))
# 
# # make a df
# d <- as.data.frame(doc.text)
# 
# write.csv(d, "./data/PoliticianOpponentData.csv")


#### Clean data ####
# This to find out how many possible politicians come from each state
congress$Tab <- 1
congByState <- dcast(congress, congress$V2 ~ congress$Tab)
congByState <- rename(congByState, c("congress$V2" = "State", "1"="CongressMembers"))
congByState <- congByState[1:50,]

# And make a list to pull the state from wikipedia
states <- states[2:53,] # the old states data
statesList <- as.character(states$NAME)


# cut out wiki surrounding info
d <- supporters[151:951,]

# make a new column to fill in below
d$State <- NA

# This will fill in the column from above with the home state of each listed politician
for(i in 1:801){
  for (j in 1:52){
    if((length(grep(statesList[j],d$doc.text[i]))) > 0) d$State[i] = statesList[j]
  }
}

d$Tab <- 1


# To differentiate between present and past politicans
d$Current <- NA
d$Current[c(8:64, 98:285, 401:418, 476:488, 520:544, 574:713)] <- "Current" 

# To differentiate between ones we count and others
# I leave out mayors, lieutenant governors & Attourneys General b/c the list is not as complete
d$Count <- NA
d$Count[c(8:64, 98:285, 401:418)] <- "Count" #eg, congress and governors
d <- d[which(d$Count == "Count"),]

# ID congress
d$Congress <- NA
d$Congress[c(1:245)] <- "Congress" 

### Make a name column
d$Name <- NA
d$Name <- str_split_fixed(d$doc.text, "[[]", n = 2)[, 1]

# To take out all of the filler without gsub (e.g. speaker of the ...)
CongressList <- as.character(congress$wikipedia_id)
CongressList <- gsub( " *\\(.*?\\) *", "", CongressList)

for(i in 1:3){
  for (j in 1:541){
    if((length(grep(CongressList[j],d$doc.text[i]))) > 0) d$Name[i] = CongressList[j]
  }
}

for(i in 58:60){
  for (j in 1:541){
    if((length(grep(CongressList[j],d$doc.text[i]))) > 0) d$Name[i] = CongressList[j]
  }
}


# Make this to combine with states
PolsByState <- dcast(d, State ~ Tab) # , sum) not working for some reason 

PolsByState <- merge(congByState, PolsByState, by="State", all=T)

PolsByState <- rename(PolsByState, c("1"="PolsWhoSupport"))
PolsByState$PolsWhoSupport[is.na(PolsByState$PolsWhoSupport)] <- 0

# I now normalize by total possible pols (that is, "counted" from above)
# +1 for the governor
PolsByState$PercOfPols <- PolsByState$PolsWhoSupport / (PolsByState$CongressMembers + 1) 


#### Map it ####
# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
#load us map data
all_states <- map_data("state")

# Prep for the merge
all_states$State <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,all_states$region, perl=TRUE)
all_states <- merge(all_states, PolsByState, by="State", all.x=T)


# Map
map <- get_map(location = "USA", zoom=3, maptype="roadmap", color = "bw")
ggmap(map)

#plot all states with ggplot
ggmap(map) +
  geom_polygon(data=all_states, aes(x=long, y=lat, group=group, fill=all_states$PercOfPols), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu"))) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Politicians Per Million")

ggsave(paste("./plots/Map.png"), dpi=300, width=6, height=5)


p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group, fill=all_states$PercOfPols), colour=NA, alpha=1) +
  scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu"))) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Politicians Per Million")
p

ggsave(paste("./plots/Map2.png"), dpi=300, width=8, height=5)



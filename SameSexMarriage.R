# Created by Daniel Hadley to analyze the support for gay marriage
# Nov, 2014
setwd("/Users/dphnrome/Documents/Git/SameSexMarriage/")

library(XML)
library(ggplot2)
library(maps)

# load data, which was scraped from wikipedia on Nov 16th, 2014
polls <- read.csv("./data/PollingData.csv")
politicians <- read.csv("./data//PoliticianData.csv")
# States data from https://www.census.gov/popest/data/state/asrh/2013/files/SCPRC-EST2013-18+POP-RES.csv
# Population for 18+
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


#### Clean data ####
states <- states[2:53,]
statesList <- as.character(states$NAME)

# cut out wiki surrounding info
d <- politicians[151:951,]

# make a new column to fill in below
d$State <- NA

# This will fill in the column from above with the home state of each listed politician
for(i in 1:801){
  for (j in 1:52){
    if((length(grep(statesList[j],d$doc.text[i]))) > 0) d$State[i] = statesList[j]
  }
}

  
  
# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p


zest <- c("New Jersey", "California")
d$test <- NA

for(i in 1:801){
  for (j in 1:2){
    if((length(grep(zest[j],d$doc.text[i]))) > 0) d$test[i] = zest[j]
  }
}

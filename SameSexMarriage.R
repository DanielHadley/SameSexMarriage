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
library(scales)

# load data, which was scraped from wikipedia on Nov 16th, 2014
polls <- read.csv("./data/PollingData.csv")
supporters <- read.csv("./data//PoliticianData.csv")
opponents <- read.csv("./data//PoliticianOpponentData.csv")

# congress data: https://www.govtrack.us/data/congress-legislators/
congress <- read.csv("./data/legislators-current.csv")
abbreviations <- read.csv("./data//us_states.csv", header=F)
congress <- merge(congress, abbreviations, by.x="state", by.y="V3", all.x=T)
remove(abbreviations)

# More congress data: https://sunlightlabs.github.io/congress/#legislator-spreadsheet
congressSunlight <- read.csv("./data//legislators-sunlight.csv")

# House data scrapped from: 
# wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives
house <- read.csv("./data//HouseOfReps.csv")
house$Name <- as.character(house$Name)
house$Name <- substring(house$Name, ((nchar(house$Name)/2)+2))

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

# # # Current House membership:
# # 
# # Read and parse HTML file
# theurl <- 'http://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives#Voting_members_by_state'
# table <-  readHTMLTable(theurl, header=T, which=6,stringsAsFactors=F)
# 
# write.csv(table, "./data/HouseOfReps.csv")


#### Clean data to make PolsByState ####
# This to find out how many possible politicians come from each state
congress$Tab <- 1
congByState <- dcast(congress, congress$V2 ~ congress$Tab)
congByState <- rename(congByState, c("congress$V2" = "State", "1"="CongressMembers"))
congByState <- congByState[1:50,]

# And make a list to pull the state from wikipedia
states <- states[2:53,] # the old states data
statesList <- as.character(states$NAME)

### First the proponents
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

d$PolsWhoSupport <- 1


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

# tab for each type of pol
d$SenatorsSupport <- 0
d$SenatorsSupport[c(1:57)] <- 1

d$RepsSupport <- 0
d$RepsSupport[c(58:245)] <- 1

d$GovsSupport <- 0
d$GovsSupport[c(246:263)] <- 1

# Make this to combine with states
meltd <- melt(d, id=c(3), measure=c(4, 9:11)) # id = non-numeric; measure = numeric
PolsByState <- dcast(meltd, State ~ variable, sum) # , sum) not working for some reason 
PolsByState <- merge(congByState, PolsByState, by="State", all=T)
PolsByState[is.na(PolsByState)] <-  0

# save a copy before moving on to opponents
# replacing the o.g. wiki data
supporters <- d


### Now the opponents
# cut out wiki surrounding info
d <- opponents[70:753,]

# make a new column to fill in below
d$State <- NA

# This will fill in the column from above with the home state of each listed politician
for(i in 1:801){
  for (j in 1:52){
    if((length(grep(statesList[j],d$doc.text[i]))) > 0) d$State[i] = statesList[j]
  }
}

d$PolsWhoOppose <- 1


# To differentiate between present and past politicans
d$Current <- NA
d$Current[c(3:44, 117:345, 487:517, 476:488, 609:623)] <- "Current" 

# To differentiate between ones we count and others
# I leave out mayors, lieutenant governors & Attourneys General b/c the list is not as complete
d$Count <- NA
d$Count[c(3:44, 117:345, 487:517)] <- "Count" #eg, congress and governors
d <- d[which(d$Count == "Count"),]

# ID congress
d$Congress <- NA
d$Congress[c(1:271)] <- "Congress" 

### Make a name column
d$Name <- NA
d$Name <- str_split_fixed(d$doc.text, "[[]", n = 2)[, 1]
d$Name <- str_split_fixed(d$Name, "[(]", n = 2)[, 1]


# To take out all of the filler without gsub (e.g. speaker of the ...)
# Use congresslist made earlier

for(i in 1:2){
  for (j in 1:541){
    if((length(grep(CongressList[j],d$doc.text[i]))) > 0) d$Name[i] = CongressList[j]
  }
}

for(i in 44:45){
  for (j in 1:541){
    if((length(grep(CongressList[j],d$doc.text[i]))) > 0) d$Name[i] = CongressList[j]
  }
}

# tab for each type of pol
d$SenatorsOppose <- 0
d$SenatorsOppose[c(1:42)] <- 1

d$RepsOppose <- 0
d$RepsOppose[c(43:271)] <- 1

d$GovsOppose <- 0
d$GovsOppose[c(272:302)] <- 1

# Make this to combine with states
meltd <- melt(d, id=c(3), measure=c(4, 9:11)) # id = non-numeric; measure = numeric
OpposeByState <- dcast(meltd, State ~ variable, sum) 
PolsByState <- merge(PolsByState, OpposeByState, by="State", all.x=T)
PolsByState[is.na(PolsByState)] <-  0


# save a copy before moving on to opponents
# replacing the o.g. wiki data
opponents <- d


# I now normalize by total possible pols (that is, "counted" from above)
# +1 for the governor
PolsByState$TotalPols <- PolsByState$CongressMembers + 1
PolsByState$PercOfPolsSupp <- PolsByState$PolsWhoSupport / PolsByState$TotalPols
PolsByState$PercOfPolsOpp <- PolsByState$PolsWhoOppose / PolsByState$TotalPols

# I comment this out so that I don't overwrite it on accident
# Maps depend on this data
# write.csv(PolsByState, "./data/PolsByState.csv")


#### Polls data ####
polls$state <- str_split_fixed(polls$State, "[[]", n = 2)[, 1]

data.m <- melt(polls, id=c(11), measure=c(4:5, 7)) # id = non-numeric; measure = numeric
PollsByState <- dcast(data.m, state ~ variable, mean)

PollsByState <- PollsByState[ which(PollsByState$state!='United States'), ]

# I comment this out so that I don't overwrite it on accident
# Maps depend on this data
# write.csv(PollsByState, "./data/PollsByState.csv")


#### Prepares data for visualizations #####
# remove data that we don't need
remove(OpposeByState, congByState, d, data.m, meltd, polls, i, j, statesList, CongressList)

d <- merge(PollsByState, PolsByState, by.x="state", by.y="State")
d <- merge(d, states, by.x="state", by.y="NAME")

# add my theme and favorite colors
lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )


summary(lm(d$PercOfPolsSupp ~ d$support))$r.squared 
ggplot(d, aes(x=d$PercOfPolsSupp, 
                y=(d$support) / 100)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  # scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Political Support Correlated to Public Opinion") + 
  xlab("Percent of State's Politicans Who Support - R-Sq=.79")+
  ylab("Percent Who Say They Support in Recent Polls") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot01.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=d$PercOfPolsOpp, 
              y=(d$X..opposition) / 100)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  # scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Political Opposition Correlated to Public Opinion") + 
  xlab("Percent of State's Politicans Who Oppose - R-Sq=.79")+
  ylab("Percent Who Say They Oppose in Recent Polls") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot02.png", dpi=300, width=5, height=4)
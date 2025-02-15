# Politicians For and Against Gay Marriage
# Created by Daniel Hadley to crate data to analyze the support for gay marriage
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

religiosity <- read.csv("./data//ReligiosityInUSA.csv")

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

# # # Religiousity:
# # 
# Source: "Mississippians Go to Church the Most; Vermonters, Least". Gallup.com. Retrieved 2012-03-17.
# # Read and parse HTML file
# theurl <- 'http://en.wikipedia.org/wiki/Religion_in_the_United_States'
# table <-  readHTMLTable(theurl, header=T, which=3,stringsAsFactors=F)
# 
# write.csv(table, "./data/ReligiosityInUSA.csv")


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
SupportersFinal <- d
# I comment this out so that I don't overwrite it on accident
# write.csv(SupportersFinal, "./data/SupportersFinal.csv")


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
OpponentsFinal <- d
# I comment this out so that I don't overwrite it on accident
# write.csv(OpponentsFinal, "./data/OpponentsFinal.csv")



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



#### Merge Supporters/Opponents with Congress Data ####
# I have not finished this

# Prepare for merge
CongressOpponents <- OpponentsFinal[which(OpponentsFinal$Congress == "Congress"),]
CongressSupporters <- SupportersFinal[which(SupportersFinal$Congress == "Congress"),]

# Prepare for Quick fixes
CongressOpponents <- data.frame(lapply(CongressOpponents, as.character), stringsAsFactors=FALSE)
CongressSupporters <- data.frame(lapply(CongressSupporters, as.character), stringsAsFactors=FALSE)

CongressOpponents[43,] = "John Boehner"
CongressOpponents[44,] = "Steve Scalise"

# Create the final df as a base
d <- congress
# Take out filler from opening parentheses on ..
d$Name <- str_split_fixed(d$wikipedia_id, "[(]", n = 2)[, 1]

# Remove leading and trailing to make them line up
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
d$Name <- trim(d$Name)
CongressOpponents$Name <- trim(CongressOpponents$Name)
CongressSupporters$Name <- trim(CongressSupporters$Name)

d$Oppose <- NA

# This will fill in the column from above with a 1 depending on whether the pol supports or not
for(i in 1:541){
  for (j in 1:271){
    if((length(grep(d$Name[i], CongressOpponents$Name[j]))) > 0) d$Oppose[i] = 1
  }
}

d$Support <- NA

# This will fill in the column from above with a 1 depending on whether the pol supports or not
for(i in 1:541){
  for (j in 1:245){
    if((length(grep(d$Name[i], CongressSupporters$Name[j]))) > 0) d$Support[i] = 1
  }
}

# This gets us part of the way there ... but ...
test <- merge(d, CongressSupporters, by="Name", all=T)
# Middle names seem to be tripping it up

## Still NAs
d$stillNA <- ifelse(((is.na(d$Oppose)) ==T) & ((is.na(d$Support)) == T), TRUE, FALSE)

# New DF
dNA <- d[which(d$stillNA == T),]
dNA$LastNameState <- paste(dNA$last_name, dNA$V2)

# Now mimick this in supports/opponents
CongressOpponents$NameFinal <-  gsub(" III","",CongressOpponents$Name)
CongressOpponents$NameFinal <-  gsub(", Jr.","",CongressOpponents$Name)
CongressOpponents$LastName <- word(CongressOpponents$Name, -1)
CongressOpponents$LastNameState <- paste(CongressOpponents$LastName, CongressOpponents$State)

CongressSupporters$NameFinal <-  gsub(" III","",CongressSupporters$Name)
CongressSupporters$NameFinal <-  gsub(", Jr.","",CongressSupporters$Name)
CongressSupporters$LastName <- word(CongressSupporters$Name, -1)
CongressSupporters$LastNameState <- paste(CongressSupporters$LastName, CongressSupporters$State)

## not working for some reason:

dNA$Oppose <- NA

# This will fill in the column from above with a 1 depending on whether the pol supports or not
for(i in 1:70){
  for (j in 1:271){
    if((length(grep(dNA$LastNameState[i], CongressOpponents$LastNameState[j]))) > 0) d$Oppose[i] = 1
  }
}

d$NASupport <- NA

# This will fill in the column from above with a 1 depending on whether the pol supports or not
for(i in 1:70){
  for (j in 1:245){
    if((length(grep(dNA$Name[i], CongressSupporters$LastNameState[j]))) > 0) d$Support[i] = 1
  }
}





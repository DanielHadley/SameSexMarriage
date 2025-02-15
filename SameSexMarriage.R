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

# Data created from MungeData script
PollsByState <- read.csv("./data//PollsByState.csv")
PolsByState <- read.csv("./data/PolsByState.csv")

SupportersFinal <- read.csv("./data//SupportersFinal.csv")
OpponentsFinal <- read.csv("./data/OpponentsFinal.csv")

religiosity <- read.csv("./data//ReligiosityInUSA.csv")



#### Prepares data for visualizations #####

d <- merge(PollsByState, PolsByState, by.x="state", by.y="State")
d <- merge(d, states, by.x="state", by.y="NAME")

d$Majority <- ifelse(d$support > 50, "Support",
                     ifelse(d$X..opposition > 50, "Oppose", "No Majority"))

# add my theme and colors that match the maps

pinkish_red = "#e74c3c"
purple = "#9b59b6"


dark_blue = "#0c2c84"
middle_blue = "#1d91c0"
teele = "#c7e9b4"
yellow = "#ffffcc"

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
                y=(d$support / 100), color=Majority)) +
  geom_point(shape=1) + #scale_x_log10() +
  scale_color_manual(values = c(purple, pinkish_red, dark_blue)) +
  geom_smooth(method=lm, color = "grey") +
  my.theme + ggtitle("Political Support Correlated to Public Opinion") + 
  xlab("Percent of State's Politicans Who Support - R-Sq=.79")+
  ylab("Percent Who Say They Support in Recent Polls") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot01.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=d$PercOfPolsOpp, 
              y=(d$X..opposition / 100), color=Majority)) +
  geom_point(shape=1) + #scale_x_log10() +
  scale_color_manual(values = c(purple, pinkish_red, dark_blue)) +
  geom_smooth(method=lm, color = "grey") +
  my.theme + ggtitle("Political Opposition Correlated to Public Opinion") + 
  xlab("Percent of State's Politicans Who Oppose - R-Sq=.79")+
  ylab("Percent Who Say They Oppose in Recent Polls") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot02.png", dpi=300, width=5, height=4)


d.top <- d[which(d$PercOfPolsOpp == 1),]
ggplot(d.top, aes(x=reorder(d.top$state, -d.top$POPEST18PLUS2013), y=d.top$POPEST18PLUS2013)) + geom_bar(colour="white", fill=middle_blue) + 
  my.theme + ggtitle("All States Where 100% of Politicans Oppose") + xlab("State")+
  ylab("18+ Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot03.png", dpi=300, width=5, height=4)


d.top <- d[which(d$PercOfPolsSupp == 1),]
ggplot(d.top, aes(x=reorder(d.top$state, -d.top$POPEST18PLUS2013), y=d.top$POPEST18PLUS2013)) + 
  geom_bar(colour="white", fill=dark_blue) + 
  my.theme + ggtitle("All States Where 100% of Politicans Support") + xlab("State")+
  ylab("18+ Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot04.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=(d$support / 100))) + geom_histogram(colour="white", fill=dark_blue, binwidth = .05) + 
  my.theme + ggtitle("Public Support for Gay Marriage") + 
  xlab("Percent of State Who Say They Support")+
  ylab("Number of States") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot05.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=d$PercOfPolsSupp)) + geom_histogram(colour="white", fill=dark_blue, binwidth = .1) + 
  my.theme + ggtitle("Political Support for Gay Marriage") + 
  xlab("% of State Politicians Who Support")+
  ylab("Number of States") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot06.png", dpi=300, width=5, height=4)


d.top <- d[which(d$GovsOppose == 1 & d$support > 50),]
ggplot(d.top, aes(x=reorder(d.top$state, -d.top$POPEST18PLUS2013), y=d.top$POPEST18PLUS2013)) + geom_bar(colour="white", fill=middle_blue) + 
  my.theme + ggtitle("Governor Opposes But the Majority Support") + xlab("State")+
  ylab("18+ Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot07.png", dpi=300, width=5, height=4)


d.top <- d[which(d$SenatorsOppose >= 1 & d$support > 50),]
ggplot(d.top, aes(x=reorder(d.top$state, -d.top$POPEST18PLUS2013), y=d.top$POPEST18PLUS2013)) + geom_bar(colour="white", fill=middle_blue) + 
  my.theme + ggtitle("A Senator Opposes But the Majority Support") + xlab("State")+
  ylab("18+ Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot08.png", dpi=300, width=5, height=4)



d <- merge(d, religiosity, by.x="state", by.y="State")
d$WeeklyAttendance <- gsub(pattern="[%]",replacement="", d$Percent)
d$WeeklyAttendance <- as.numeric(d$WeeklyAttendance)
summary(lm(d$X..opposition ~ d$WeeklyAttendance))$r.squared 
ggplot(d, aes(x=(d$X..opposition / 100), 
              y=(d$WeeklyAttendance / 100), color=Majority)) +
  geom_point(shape=1) + #scale_x_log10() +
  scale_color_manual(values = c(purple, pinkish_red, dark_blue)) +
  geom_smooth(method=lm, color = "grey") +
  my.theme + ggtitle("Public Opinion Correlated to Religiosity") + 
  xlab("Percent Who Say They Oppose Gay Marriage - R-Sq=.68")+
  ylab("Percent Who Say They Attend Church Weekly") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot09.png", dpi=300, width=5, height=4)




t <- table(congress$V2, congress$gender)
ratio <- as.data.frame.matrix(t)
ratio$percentMale <- ratio$M / (ratio$M + ratio$F)
d <- merge(d, ratio, by.x="state", by.y="row.names")
d$PerOfCongOpp <- (d$SenatorsOppose + d$RepsOppose) / d$CongressMembers
d$MaleCongress <- ifelse(d$percentMale.x == 1, TRUE, FALSE)
t <- aggregate(d$PerOfCongOpp ~ d$MaleCongress, d, mean ) # makes a two-way table

ggplot(t, aes(x=t$"d$MaleCongress", y=t$"d$PerOfCongOpp")) + geom_bar(colour="white", fill=middle_blue) + 
  my.theme + ggtitle("All-Male Delegations More Likely To Oppose") + xlab("All Male Congressional Delegation")+
  ylab("Percent of Congress Members Who Oppose") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot10.png", dpi=300, width=5, height=4)




summary(lm(d$PercOfPolsSupp ~ d$support))$r.squared 
ggplot(d, aes(x=d$PercOfPolsSupp, 
              y=(d$support / 100))) +
  geom_point(shape=1) + #scale_x_log10() +
  scale_color_manual(values = c(purple, pinkish_red, dark_blue)) +
  geom_smooth(method=lm, color = "grey") +
  my.theme + ggtitle("Political Support Correlated to Public Opinion") + 
  xlab("Percent of State's Politicans Who Support - R-Sq=.79")+
  ylab("Percent Who Say They Support in Recent Polls") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave("./plots/plot11.png", dpi=300, width=5, height=4)


d$GovSenatorsOppo <- d$GovsOppose + d$SenatorsOppose
d.top <- d[which(d$GovSenatorsOppo > 0 & d$X..opposition < 50),]
ggplot(d.top, aes(x=reorder(d.top$state, -d.top$X..opposition), y=(d.top$X..opposition/100))) + geom_bar(colour="white", fill=middle_blue) + 
  my.theme + ggtitle("A Statewide Politician Opposes But Public Opposition < 50%") + 
  xlab("State With at Least One Senator or Governor Opposed")+
  ylab("Percent Who Say They Oppose in Recent Polls") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot12.png", dpi=300, width=5, height=4)

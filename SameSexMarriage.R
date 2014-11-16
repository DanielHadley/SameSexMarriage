library(XML)

# Read and parse HTML file
doc.html = htmlTreeParse('http://en.wikipedia.org/wiki/List_of_supporters_of_same-sex_marriage_in_the_United_States#Elected_officials',
                         useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.text = unlist(xpathApply(doc.html, '//li', xmlValue))

# make a df
d <- as.data.frame(doc.text)

# Extract stuff in parentheses
d$place = gsub("[\\(\\)]", "", regmatches(d$doc.text, gregexpr("(?=\\().*?(?<=\\))", d$doc.text, perl=T)))

d$place = gsub("[\\(\\)]", "", regmatches(d$doc.text, gregexpr("(?<=\\().*?(?=\\))", d$doc.text, perl=T)))

# http://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r-regex

zest <- c("New Jersey", "California")

# for (i in 1:2 ) {
#   d$test <- ifelse((grepl("^[^_]+test[i]",d$doc.text)) = TRUE, test[i], "nope")
# }
# 
# for (i in 1:2 ) {
#   d$test <- ifelse(grepl("^[^_]+California",d$doc.text") = TRUE, "yep", "nope")
# }

# grepl("^[^_]+California",d$doc.text)

if(length(grep(test[2],"Californ"))>0) print(test[2]) else print("Not found")

for(i in 1:2){
  if(length(grep(test[i],"California"))>0) print(test[i]) else print("Not found")
}

for(i in 1:2){
  d$test <- if(length(grep(test[i],d$doc.text))>0) print(test[i]) else print("Not found")
}

for(i in 1:2){
  d$testh[1] <- ifelse((length(grep(test[i],d$doc.text[1]))) > 0, test[i], "not found")
}

for(i in 1:1529){
  for (j in 1:2){
    d$test[i] <- ifelse((length(grep(zest[j],d$doc.text[i]))) > 0, zest[j], "not found")
  }
}


for(i in 1:1529){
  for (j in 1:2){
    if((length(grep(zest[j],d$doc.text[i]))) > 0) d$test[i] = zest[j]
  }
}



# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
library(ggplot2)
library(maps)
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p



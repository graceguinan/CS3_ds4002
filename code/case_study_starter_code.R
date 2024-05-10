
# load important packages
library(schrute)
library(dplyr)
library(vader)
library(ggplot2)

# load data
mydata <- schrute::theoffice
dplyr::glimpse(mydata)

# figure out characters w most lines
x <- table(mydata$character)
top_characters = as.data.frame(x[x > 100])
top_characters[order(-top_characters$Freq),]

# create plot of top 4 characters amount of lines by season
michael = mydata[mydata$character == "Michael",]
dwight = mydata[mydata$character == "Dwight",]
jim = mydata[mydata$character == "Jim",]
pam = mydata[mydata$character == "Pam",]
d1 = michael%>%group_by(season)%>%count()
d1$character = replicate(8, "Michael")
d2 = dwight%>%group_by(season)%>%count()
d2$character = replicate(9, "Dwight")
d3 = pam%>%group_by(season)%>%count()
d3$character = replicate(9, "Pam")
d4 = jim%>%group_by(season)%>%count()
d4$character = replicate(9, "Jim")
dat_new = rbind(d1, d2, d3, d4)
dat_new$gender = ifelse(dat_new$character == "Pam", "f", "m")
ggplot(dat_new, aes(x = as.factor(season), y = n, fill = gender))+
  geom_bar(stat="identity")+facet_wrap(~character)+xlab("Season Number")+
  ggtitle("Number of lines per season (top 4 characters with the most lines)")+  scale_fill_manual(values=c("red", "blue"))

# okay, now that some preliminary analysis has been completed, it's time...
# run VADER here! (using get_vader function)




# now, analyze your results!
# statistical analysis, create graphs, etc
# be creative!







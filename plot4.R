#################################################################################################
# Course Project 2 - Question 4
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?
#
#


# Here the main problem was to find all the coal combustion-related sources among the SCC.
# After having seen the complete list (http://www.state.nj.us/dep/aqm/es/scc.pdf), I took the 
# decision to explore from more general to more specific sources, looking for those words: 
#
#                 coal, lignite, coke, carbon, charcoal
#
# I extracted all the descriptions containing at least one of those words (s3 and s4 character vectors)
# and then the corresponding SCC levels (scc3 and scc4 data frames). 
# Afterwords, I made a union between the two SCC subsets to have the final 
# coal combustion-related sources' SCC set (coalRelatedScc).
#################################################################################################



rm(list=ls())
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

sourceLevels3 = levels(SCC$SCC.Level.Three)
selectedLevelThree = grep("[Cc]oal|[Ll]ignite|[Cc]oke|[Cc]arbon |[Cc]harcoal", sourceLevels3)
s3 = sourceLevels3[selectedLevelThree]

sourceLevels4 = levels(SCC$SCC.Level.Four)
selectedLevelFour = grep("[Cc]oal|[Ll]ignite|[Cc]oke|[Cc]arbon |[Cc]harcoal", sourceLevels4)
s4 = sourceLevels4[selectedLevelFour]

scc3 = subset(SCC, SCC.Level.Three %in% s3, select=c(SCC, SCC.Level.Three))
scc4 = subset(SCC, SCC.Level.Four %in% s4, select=c(SCC, SCC.Level.Four))

coalRelatedScc = union(scc3$SCC, scc4$SCC)

# take into account only data coming from specific SCCs
coalSccData = subset(NEI, SCC %in% coalRelatedScc, select=c(Emissions, year))

# total emissions by year
coalSccDataSum = as.data.frame(ddply(coalSccData, ~ year, summarise, tot=sum(Emissions)))


# bar graphs
png(filename="plot4.png", width = 800, height = 800)
ggplot(data=coalSccDataSum, aes(x=factor(year), y=tot/1000, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) +
  guides(fill=FALSE) +
  ggtitle("Total US Coal Combustion-Related Sources "~PM[2.5]~" Emissions") +
  xlab("Year") + ylab("Emissions (thousands of tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))
dev.off()
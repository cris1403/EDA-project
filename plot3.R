#######################################################################################################
# Course Project 2 - Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# I used a bar plot instead of a line graph because - having one value every 3 years - I don't consider
# the Emissions variable as continous. 
# Checking the 4 facets, it's easy to see which kind of emissions increased or decreased from 1999-2008. 
########################################################################################################

rm(list=ls())

packages <- c("ggplot2","plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

NEI = readRDS("summarySCC_PM25.rds")
NEI$type = factor(NEI$type)

# take into account only data about Baltimore City
baltimore = subset(NEI, fips == "24510")

# total emissions by year and type
dataset = as.data.frame(ddply(baltimore, .(year,type), summarise, tot=sum(Emissions)))

# bar graphs
png(filename="plot3.png", width = 800, height = 800)
ggplot(data=dataset, aes(x=factor(year), y=tot, group=1, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) + facet_grid(. ~ type) +
  guides(fill=FALSE) +
  ggtitle("Total Baltimore City PM2.5 Emissions by Type of Source") +
  xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))
dev.off()
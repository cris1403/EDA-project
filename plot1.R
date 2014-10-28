#################################################################################################
# Course Project 2 - Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
#
# I used a bar plot instead of a line graph because having data every 3 years, I don't consider
# the Emissions variable as continous because I don't know its values for every year.
#################################################################################################

rm(list=ls())
library(plyr)

NEI <- readRDS("summarySCC_PM25.rds")

# total emissions by year
dataset = as.data.frame(ddply(NEI, ~ year, summarise, tot=sum(Emissions)))

# create the plot using the base plotting system
png(filename="plot1.png", width = 800, height = 800)
barplot(dataset$tot/1000, col="orange",
        main="Total US "~PM[2.5]~" Emissions", xlab="Year", ylab="Emissions (thousands of tons)",
        cex.lab=1.3, cex.axis=1, cex.main=1.4,
        names.arg = c("1999", "2002", "2005", "2008"))
dev.off()

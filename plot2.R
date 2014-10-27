#################################################################################################
# Course Project 2 - Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#
# I used a bar plot instead of a line graph because - having one value every 3 years - I don't consider
# the Emissions variable as continous. In Baltimore, emissions went down from 1999.
#################################################################################################

rm(list=ls())

if (length(setdiff("plyr", rownames(installed.packages()))) > 0) {
  install.packages(setdiff("plyr", rownames(installed.packages())))  
}

NEI = readRDS("summarySCC_PM25.rds")
baltimore = subset(NEI, fips == "24510")      # look at baltimore city emissions only!

# Total emissions by year in Baltimore City
dataset = as.data.frame(ddply(baltimore, ~ year, summarise, tot=sum(Emissions)))

# create the plot using the base plotting system
png(filename="plot2.png", width = 800, height = 800)
barplot(dataset$tot, col="orange",
        main="Total Baltimore City PM2.5 Emissions", xlab="Year", ylab="Emissions (tons)",
        cex.lab=1.3, cex.axis=1, cex.main=1.4,
        names.arg = c("1999", "2002", "2005", "2008"))
dev.off()



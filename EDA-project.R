rm(list=ls())

setInternet2(use = TRUE) 

# download the zip file for the project
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip?accessType=DOWNLOAD"

td = tempdir()                            # create a temporary directory
tf = tempfile(tmpdir=td, fileext=".zip")  # create a tempfile

download.file(fileUrl, destfile=tf, mode="wb")    # download the zip file
fname = unzip(tf, list=TRUE)$Name                 # get the name of the files in the zip archive
unzip(tf, files=fname, exdir=".", overwrite=TRUE) # unzip the file to a directory

rm(td, tf)                                # remove unnecessary stuff

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$fips = factor(NEI$fips)
NEI$SCC = factor(NEI$SCC)
NEI$Pollutant = factor(NEI$Pollutant)


# 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
library(plyr)
dataset = as.data.frame(ddply(NEI, ~ year, summarise, tot=sum(Emissions)))

png(filename="plot1.png", width = 480, height = 480)
plot(dataset$year, dataset$tot/1000, type="b", pch=19, cex=2, col="blue",
     main="Total US PM2.5 Emissions", xlab="Year", ylab="Emissions (thousands of tons)",
     xaxt="n", cex.lab=1.2, cex.axis=1, cex.main=1.3)
axis(side=1, at=c("1999", "2002", "2005", "2008"))
dev.off()

barplot(dataset$tot/1000, col="blue",
        main="Total US PM2.5 Emissions", xlab="Year", ylab="Emissions (thousands of tons)",
        xaxt="n", cex.lab=1.2, cex.axis=1, cex.main=1.3,
        names.arg = c("1999", "2002", "2005", "2008"))



# 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
library(plyr)

baltimore = subset(NEI, fips == "24510")
dataset = as.data.frame(ddply(baltimore, ~ year, summarise, tot=sum(Emissions)))

png(filename="plot2.png", width = 480, height = 480)
plot(dataset$year, dataset$tot, type="b", pch=19, cex=2, col="red",
     main="Total Baltimore City PM2.5 Emissions", xlab="Year", ylab="Emissions (tons)",
     xaxt="n", cex.lab=1.2, cex.axis=1, cex.main=1.2)
axis(side=1, at=c("1999", "2002", "2005", "2008"))
dev.off()

# 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
library(plyr)

NEI$type = factor(NEI$type)
baltimore = subset(NEI, fips == "24510")
dataset = as.data.frame(ddply(baltimore, .(year,type), summarise, tot=sum(Emissions)))

g <- ggplot(dataset, aes(factor(year), tot, group=1))

# line graphs
ggplot(dataset, aes(factor(year), tot, group=1)) + 
  geom_point(colour="blue", size=4, shape=21, fill="red") + 
  facet_grid(. ~ type) +    
  geom_line(colour="blue", linetype="dotted", size=1) +
  ggtitle("Total Baltimore City PM2.5 Emissions by Type") +
  xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

# bar graphs
ggplot(data=dataset, aes(x=factor(year), y=tot/1000, group=1, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) + facet_grid(. ~ type) +
  guides(fill=FALSE) +
  ggtitle("Total US Coal Combustion-Related Sources PM2.5 Emissions") +
  xlab("Year") + ylab("Emissions (thousands of tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))


# 4
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?
library(ggplot2)
library(plyr)

sourceLevels3 = levels(SCC$SCC.Level.Three)
selectedLevelThree = grep("[Cc]oal|[Ll]ignite|[Cc]oke|[Cc]arbon |[Cc]harcoal", sourceLevels3)
s3 = sourceLevels3[selectedLevelThree]

sourceLevels4 = levels(SCC$SCC.Level.Four)
selectedLevelFour = grep("[Cc]oal|[Ll]ignite|[Cc]oke|[Cc]arbon |[Cc]harcoal", sourceLevels4)
s4 = sourceLevels4[selectedLevelFour]

scc3 = subset(SCC, SCC.Level.Three %in% s3, select=c(SCC, SCC.Level.Three))
scc4 = subset(SCC, SCC.Level.Four %in% s4, select=c(SCC, SCC.Level.Four))

coalRelatedScc = union(scc3$SCC, scc4$SCC)

coalSccData = subset(NEI, SCC %in% coalRelatedScc, select=c(Emissions, year))

coalSccDataSum = as.data.frame(ddply(coalSccData, ~ year, summarise, tot=sum(Emissions)))


# bar graphs
ggplot(data=coalSccDataSum, aes(x=factor(year), y=tot/1000, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) +
  guides(fill=FALSE) +
  ggtitle("Total US Coal Combustion-Related Sources PM2.5 Emissions") +
  xlab("Year") + ylab("Emissions (thousands of tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))




# 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
library(ggplot2)
library(plyr)

baltimore = subset(NEI, fips == "24510")

sourceEI = levels(SCC$EI.Sector)
selectedEILevels = grep("[Mm]otor|[Vv]ehicles", sourceEI)
sei = sourceEI[selectedEILevels]

sourceLevels3 = levels(SCC$SCC.Level.Three)
selectedLevelThree = grep("[Mm]otor|[Vv]ehicles", sourceLevels3)
s3 = sourceLevels3[selectedLevelThree]

sourceLevels4 = levels(SCC$SCC.Level.Four)
selectedLevelFour = grep("[Mm]otor|[Vv]ehicles", sourceLevels4)
s4 = sourceLevels4[selectedLevelFour]

sccei = subset(SCC, EI.Sector %in% sei, select=c(SCC, EI.Sector))
scc3 = subset(SCC, SCC.Level.Three %in% s3, select=c(SCC, SCC.Level.Three))
scc4 = subset(SCC, SCC.Level.Four %in% s4, select=c(SCC, SCC.Level.Four))

motorVehicleRelatedScc = union(union(sccei$SCC, scc3$SCC), scc4$SCC)

vehicleSccData = subset(baltimore, SCC %in% motorVehicleRelatedScc, select=c(Emissions, year))
vehicleSccDataSum = as.data.frame(ddply(vehicleSccData, ~ year, summarise, tot=sum(Emissions)))

# bar graphs
ggplot(data=vehicleSccDataSum, aes(x=factor(year), y=tot, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) +
  guides(fill=FALSE) +
  ggtitle("Total Baltimore Motor Vehicles-Related Sources PM2.5 Emissions") +
  xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))



# 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
library(ggplot2)
library(plyr)

baltimore = subset(NEI, fips == "24510")
lacounty = subset(NEI, fips == "06037")

sourceEI = levels(SCC$EI.Sector)
selectedEILevels = grep("[Mm]otor|[Vv]ehicles", sourceEI)
sei = sourceEI[selectedEILevels]

sourceLevels3 = levels(SCC$SCC.Level.Three)
selectedLevelThree = grep("[Mm]otor|[Vv]ehicles", sourceLevels3)
s3 = sourceLevels3[selectedLevelThree]

sourceLevels4 = levels(SCC$SCC.Level.Four)
selectedLevelFour = grep("[Mm]otor|[Vv]ehicles", sourceLevels4)
s4 = sourceLevels4[selectedLevelFour]

sccei = subset(SCC, EI.Sector %in% sei, select=c(SCC, EI.Sector))
scc3 = subset(SCC, SCC.Level.Three %in% s3, select=c(SCC, SCC.Level.Three))
scc4 = subset(SCC, SCC.Level.Four %in% s4, select=c(SCC, SCC.Level.Four))

motorVehicleRelatedScc = union(union(sccei$SCC, scc3$SCC), scc4$SCC)

vehicleSccBaltimore = subset(baltimore, SCC %in% motorVehicleRelatedScc, select=c(fips, Emissions, year))
vehicleSccLACounty = subset(lacounty, SCC %in% motorVehicleRelatedScc, select=c(fips, Emissions, year))
vehicleSccData = rbind(vehicleSccBaltimore, vehicleSccLACounty)

vehicleSccDataSum = as.data.frame(ddply(vehicleSccData, .(year,fips), summarise, tot=sum(Emissions)))

# line graphs
ggplot(data=vehicleSccDataSum, aes(x=factor(year), y=tot, group=fips, colour=fips)) + 
  geom_line(colour="blue", linetype="dotted", size=1) + 
  geom_point(colour="blue", size=4, shape=21, fill="red") +
  ggtitle("Motor Vehicles PM2.5 Emissions - Baltimore vs LA County") +
  xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))


library(quantmod)
changeOverTime= ddply(vehicleSccDataSum, "fips", transform,  DeltaCol = Delt(tot,k=1)*100)
names(changeOverTime) = c('year','fips','tot','percchange')
changeOverTime[1,4]=0
changeOverTime[5,4]=0
changeOverTime$fips =revalue(changeOverTime$fips, c("06037"="Los Angeles County", "24510"="Baltimore City"))

# bar graphs
ggplot(data=changeOverTime, aes(x=factor(year), y=percchange, group=1, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) + facet_grid(. ~ fips) +
  guides(fill=FALSE) +
  ggtitle("Motor Vehicles PM2.5 Emissions - Changes over Time") +
  xlab("Year") + ylab("Changes over time") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"),
        strip.text.x = element_text(size = 12))


###############################################################################################################
# Course Project 2 - Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
#
# First of all I found all the SCC related to motor vehicles. I explored from more general to more specific 
# sources, looking for two words: motor, vehicles.
#
# I collected all the descriptions containing at least one of these words (along the R script, search for 
# "sei", "s3" and "s4" character vectors) and gathered the corresponding SCC levels ("scei","scc3" and "scc4" 
# data frames). 
# Afterwords, I made a union between the three SCC subsets ("motorVehicleRelatedScc"). From the main NEI dataset,
# I took into account only data related to this "motorVehicleRelatedScc" subset in Baltimore and finally 
# computed total emissions by year.
#
# I used a bar plot instead of a line graph because - having one value every 3 years - I don't consider
# the Emissions variable as continous. From the bar chart, we see that in Baltimore there a strong decrease 
# in emissions from motor vehicle-related sources from 1999-2008.
############################################################################################################

rm(list=ls())

packages <- c("ggplot2","plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

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
png(filename="plot5.png", width = 800, height = 800)
ggplot(data=vehicleSccDataSum, aes(x=factor(year), y=tot, fill=year)) +
  geom_bar(colour="black", stat="identity", width=0.7) +
  guides(fill=FALSE) +
  ggtitle("Total Baltimore Motor Vehicles-Related Sources PM2.5 Emissions") +
  xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face="bold"))
dev.off()
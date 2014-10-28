###############################################################################################################
# Course Project 2 - Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
#
# First of all I found all the SCC related to motor vehicles (SCC different levels containing the words
# "motor" or "vehicle". From the main NEI dataset, I took into account only data related to this 
# "motorVehicleRelatedScc" subset in Baltimore City and LA County and finally computed total emissions by year.
# 
# Here the question is about greater changes over time, so we both check absolute values and
# percentage changes in emissions in both areas. 
# We note different order of magnitude in the two areas: in the whole LA County there are much higher emissions
# absolute values. But we also see that in Baltimore there are much higher changes over time. Here the 
# emissions are getting lower and lower across time. In LA County went up and down, with a slow increase from
# 1999 to 2008.
############################################################################################################
rm(list=ls())

packages <- c("ggplot2","plyr","grid","gridExtra","quantmod")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

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

# remove useles datasets
rm(NEI, SCC, baltimore, lacounty, scc3, scc4, sccei)
vehicleSccDataSum$fips = factor(vehicleSccDataSum$fips)
vehicleSccDataSum$fips = revalue(vehicleSccDataSum$fips, c("06037"="Los Angeles County", "24510"="Baltimore City"))


require(quantmod)
changeOverTime= ddply(vehicleSccDataSum, "fips", transform,  DeltaCol = Delt(tot,k=1)*100)
names(changeOverTime) = c('year','fips','tot','percchange')
changeOverTime[1,4]=0
changeOverTime[5,4]=0

# line graph: absolute values
p1 <- ggplot(data=vehicleSccDataSum, aes(x=factor(year), y=tot, group=fips, colour=fips)) + 
      geom_line(linetype="dotted", size=1) + 
      geom_point(size=4, shape=19) +
      ggtitle("Absolute values") +
      xlab("Year") + ylab("Emissions (tons)") + theme_bw() +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=13, face="bold"),
        plot.title=element_text(size=15, face="bold"), legend.position="top", legend.title=element_blank())

# bar graphs: percentage changes
p2 <- ggplot(data=changeOverTime, aes(x=factor(year), y=percchange, group=1, fill=year)) +
      geom_bar(colour="black", stat="identity", width=0.7) + facet_grid(. ~ fips) +
      guides(fill=FALSE) +
      ggtitle("Changes over Time") +
      xlab("Year") + ylab("% change") + theme_bw() +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=13, face="bold"),
            plot.title=element_text(size=15, face="bold"), strip.text.x = element_text(size = 12))

png(filename="plot6.png", width = 800, height = 800)
grid.arrange(p1, p2, ncol = 2, main = textGrob("Motor Vehicles "~PM[2.5]~" Emissions", gp=gpar(fontsize=16,font=2)))
dev.off()
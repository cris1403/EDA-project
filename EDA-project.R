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
NEI$year = factor(NEI$year)


# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
library(plyr)
dataset = as.data.frame(ddply(NEI, ~yea, summarise, tot=sum(Emissions)))

plot(dataset$year, dataset$tot, type="p", col="red")

plot_1 <- aggregate(Emissions ~ year, NEI, sum)
plot(plot_1$year,plot_1$Emissions, main="Total US PM2.5 Emissions", "b", xlab="Year", ylab="Emissions (thousands of tons)",xaxt="n")
axis(side=1, at=c("1999", "2002", "2005", "2008"))
plot(dataset$year, dataset$tot, main="Total US PM2.5 Emissions", type="b", xlab="Year", ylab="Emissions (thousands of tons)",xaxt="n")
axis(side=1, at=c("1999", "2002", "2005", "2008"))

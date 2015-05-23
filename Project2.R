# setwd("PUT YOUR SETWD WHERE THE summarySCC_PM25.rds and Source_Classification_Code.rds are.") 

# 1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
aggdata = aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum, na.rm=TRUE)
names(aggdata) = c("year", "Emissions")
png('plot1.png')
barplot(height=aggdata$Emissions, names.arg=aggdata$year, ylab="Emission from PM2.5", xlab="Year")
dev.off()

# 2 Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
baltimoreData = NEI[NEI$fips=="24510", ]
aggdata = aggregate(baltimoreData$Emissions, by=list(baltimoreData$year), FUN=sum, na.rm=TRUE)
names(aggdata) = c("year", "Emissions")
png('plot2.png')
barplot(height=aggdata$Emissions, names.arg=aggdata$year, ylab="Emission from PM2.5 - Baltimore MA", xlab="Year")
dev.off()

# 3 Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)
baltimoreData = NEI[NEI$fips=="24510", ]
p = ggplot(data=baltimoreData, aes(x=year, y=Emissions)) + 
  facet_grid(. ~ type) +  
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Emission from PM2.5 - Baltimore MA")
ggsave(p, file="plot3.png", width=10, height=4) 

# 4 Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEISCC = merge(NEI, SCC, by="SCC")
coalRows = grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
coalData = NEISCC[coalRows, ]
aggdata = aggregate(coalData$Emissions, by=list(coalData$year), FUN=sum, na.rm=TRUE)
names(aggdata) = c("year", "Emissions")
p = ggplot(data=aggdata, aes(x=year, y=Emissions)) + 
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Emission from coal combustion")
ggsave(p, file="plot4.png") 

# 5 How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
roadBaltimoreVehiclesData = NEI[NEI$type=="ON-ROAD" & NEI$fips=="24510", ]
aggdata = aggregate(roadBaltimoreVehiclesData$Emissions, by=list(roadBaltimoreVehiclesData$year), FUN=sum, na.rm=TRUE)
names(aggdata) = c("year", "Emissions")
p = ggplot(data=aggdata, aes(x=year, y=Emissions)) + 
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Emission from on road vehicles in Baltimore")
ggsave(p, file="plot5.png") 

# 6 Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
roadBaltimoreVehiclesData = NEI[NEI$type=="ON-ROAD" & NEI$fips=="24510", ]
aggdataBaltimore = aggregate(roadBaltimoreVehiclesData$Emissions, by=list(roadBaltimoreVehiclesData$year), FUN=mean)
names(aggdataBaltimore) = c("year", "MeanEmission")
aggdataBaltimore$County="Baltimore"

roadLAVehiclesData = NEI[NEI$type=="ON-ROAD" & NEI$fips=="06037", ]
aggdataLA = aggregate(roadLAVehiclesData$Emissions, by=list(roadLAVehiclesData$year), FUN=mean)
names(aggdataLA) = c("year", "MeanEmission")
aggdataLA$County="L.A."

aggdata = rbind(aggdataBaltimore, aggdataLA)

p = ggplot(data=aggdata, aes(x=year, y=MeanEmission, colour=County) ) + 
  geom_line() +
  labs(x = "Year", y = "Mean of on-road vehicles emission from PM2.5 ")

ggsave(p, file="plot6.png") 
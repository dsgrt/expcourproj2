# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

neiyearstotals <- neiyears[,c(4,6)]
neiyearstotals <- group_by(neiyearstotals, year)
neiyearstotals <- neiyearstotals %>% summarise_each(funs(sum))

# Plot neiyearstotals
plot(neiyearstotals$year, neiyearstotals$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions")
axis(1, at = seq(1999, 2008, by = 3))
lines(neiyearstotals)
modelplot1 <- lm(Emissions ~ year, neiyearstotals)
abline(modelplot1, lwd = 1, col = "red")
dev.copy(png, file = "plot1.png")
dev.off()



# Plot 2
neibalt <- nei[nei$fips == "24510", ]
neibalttotals <- neibalt[,c(4,6)]
neibalttotals <- group_by(neibalttotals, year)
neibalttotals <- neibalttotals %>% summarise_each(funs(sum))

plot(neibalttotals$year, neibalttotals$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions")
axis(1, at = seq(1999, 2008, by = 3))
lines(neibalttotals)
modelplot2 <- lm(Emissions ~ year, neibalttotals)
abline(modelplot2, lwd = 1, col = "red")
dev.copy(png, file = "plot2.png")
dev.off()

#Plot 3
neibaltbytype <- neibalt[ ,c(4:6)]
neibaltbytype <- group_by(neibaltbytype, type, year)
neibaltbytype <- neibaltbytype %>% summarise_each(funs(sum))

library(ggplot2)

qplot(neibaltbytype$year, neibaltbytype$Emissions, group = neibaltbytype$type, color = neibaltbytype$type, ylab = "PM2.5 Emissions", xlab = "Year") + geom_point() + geom_line() + geom_smooth(method="lm", fill=NA, lwd = .5, linetype = "dotted") + ylim(0, 2200) + guides(color=guide_legend(title="Type"))
dev.copy(png, file = "plot3.png")
dev.off()


# Plot 4

plot4scc <- subset(scc, grepl("coal", EI.Sector, ignore.case = TRUE))
plot4sccunique <- merge(plot4scc, nei)
plot4sccunique <- plot4sccunique[,c("Emissions", "year")]
plot4sccunique <- group_by(plot4sccunique, year)
plot4sccunique <- plot4sccunique %>% summarise_each(funs(sum))

plot(plot4sccunique$year, plot4sccunique$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions from coal-related sources")
axis(1, at = seq(1999, 2008, by = 3))
lines(plot4sccunique)
modelplot3 <- lm(Emissions ~ year, plot4sccunique)
abline(modelplot3, lwd = 1, col = "red")
dev.copy(png, file = "plot4.png")
dev.off()

# Plot 5

plot5scc <- subset(scc, grepl("vehicle", EI.Sector, ignore.case = TRUE))
plot5sccunique <- merge(plot5scc, nei)
plot5sccunique <- plot5sccunique[plot5sccunique$fips == "24510", ]
plot5sccunique <- plot5sccunique[,c("Emissions", "year")]
plot5sccunique <- group_by(plot5sccunique, year)
plot5sccunique <- plot5sccunique %>% summarise_each(funs(sum))

plot(plot5sccunique$year, plot5sccunique$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions from motor vehicle sources")
axis(1, at = seq(1999, 2008, by = 3))
lines(plot5sccunique)
modelplot5 <- lm(Emissions ~ year, plot5sccunique)
abline(modelplot5, lwd = 1, col = "red")
dev.copy(png, file = "plot5.png")
dev.off()

# Plot 6

plot6scc <- subset(scc, grepl("vehicle", EI.Sector, ignore.case = TRUE))
plot6sccunique <- merge(plot6scc, nei)
plot6sccunique <- plot6sccunique[plot6sccunique$fips == c("24510","06037"), ]

plot6sccunique <- plot6sccunique[,c("Emissions", "year", "fips")]
plot6sccunique <- group_by(plot6sccunique, year, fips)
plot6sccunique <- plot6sccunique %>% summarise_each(funs(sum))

qplot(plot6sccunique$year, plot6sccunique$Emissions, group = plot6sccunique$fips, color = plot6sccunique$fips, ylab = "PM2.5 Emissions from motor veh. sources", xlab = "Year") + geom_point() + geom_line() + geom_smooth(method="lm", fill=NA, lwd = .5, linetype = "dotted") + ylim(0, 4000) + guides(color=guide_legend(title="US County")) + scale_color_manual(values = c("red", "blue"), labels = c("Los Angeles", "Baltimore")) 
dev.copy(png, file = "plot6.png")
dev.off()

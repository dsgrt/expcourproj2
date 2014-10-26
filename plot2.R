# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Subset NEI data by county code for Baltimore
neibalt <- nei[nei$fips == "24510", ]

# Sum total emissions by year
neibalttotals <- neibalt[,c(4,6)]
neibalttotals <- group_by(neibalttotals, year)
neibalttotals <- neibalttotals %>% summarise_each(funs(sum))

# Plot total emissions in Baltimore by year
plot(neibalttotals$year, neibalttotals$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions")
axis(1, at = seq(1999, 2008, by = 3))
lines(neibalttotals)
modelplot2 <- lm(Emissions ~ year, neibalttotals)
abline(modelplot2, lwd = 1, col = "red")
title(main = "Total PM2.5 emissions in Baltimore county from 1999 to 2008")

# Create PNG of plot
dev.copy(png, file = "plot2.png")
dev.off()
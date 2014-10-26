# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Sum total emissions by year
neiyearstotals <- nei[,c(4,6)]
neiyearstotals <- group_by(neiyearstotals, year)
neiyearstotals <- neiyearstotals %>% summarise_each(funs(sum))

# Plot total emissions in US by year
plot(neiyearstotals$year, neiyearstotals$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions")
axis(1, at = seq(1999, 2008, by = 3))
lines(neiyearstotals)
modelplot1 <- lm(Emissions ~ year, neiyearstotals)
abline(modelplot1, lwd = 1, col = "red")
title(main = "Total PM2.5 emissions in the US from 1999 to 2008")

# Create PNG of plot
dev.copy(png, file = "plot1.png")
dev.off()
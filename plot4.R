# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Subset SCC data by sources of "coal" in the "EI.Sector" column
plot4scc <- subset(scc, grepl("coal", EI.Sector, ignore.case = TRUE))

# Merge SCC data subset with NEI data
plot4sccunique <- merge(plot4scc, nei)
plot4sccunique <- plot4sccunique[,c("Emissions", "year")]

# Sum total emissions by year for coal sources
plot4sccunique <- group_by(plot4sccunique, year)
plot4sccunique <- plot4sccunique %>% summarise_each(funs(sum))

# Plot total emissions by year for coal sources
plot(plot4sccunique$year, plot4sccunique$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions from coal-related sources")
axis(1, at = seq(1999, 2008, by = 3))
lines(plot4sccunique)
modelplot3 <- lm(Emissions ~ year, plot4sccunique)
abline(modelplot3, lwd = 1, col = "red")
title(main = "Total PM2.5 emissions from coal-combustion related sources \nin the US from 1999 to 2008")

# Create PNG of plot
dev.copy(png, file = "plot4.png")
dev.off()
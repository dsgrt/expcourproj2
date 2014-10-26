# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Subset SCC data by "vehicle" sources of in the "EI.Sector" column
plot5scc <- subset(scc, grepl("vehicle", EI.Sector, ignore.case = TRUE))

# Merge SCC data subset with NEI data
plot5sccunique <- merge(plot5scc, nei)

# Subset for Baltimore county data
plot5sccunique <- plot5sccunique[plot5sccunique$fips == "24510", ]

# Keep columns with emissions and year data
plot5sccunique <- plot5sccunique[,c("Emissions", "year")]

# Group data and sum emissions by year
plot5sccunique <- group_by(plot5sccunique, year)
plot5sccunique <- plot5sccunique %>% summarise_each(funs(sum))

# Plot emissions in Baltimore city from motor vehicle sources by year
plot(plot5sccunique$year, plot5sccunique$Emissions, xaxt = "n", xlab = "Year", ylab = "Total PM2.5 Emissions from motor vehicle sources")
axis(1, at = seq(1999, 2008, by = 3))
lines(plot5sccunique)
modelplot5 <- lm(Emissions ~ year, plot5sccunique)
abline(modelplot5, lwd = 1, col = "red")
title(main = "Total PM2.5 emissions from motor vehicle related sources \nin Baltimore from 1999 to 2008")


# Create PNG of plot
dev.copy(png, file = "plot5.png")
dev.off()
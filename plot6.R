# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Subset SCC data by "vehicle" sources of in the "EI.Sector" column
plot6scc <- subset(scc, grepl("vehicle", EI.Sector, ignore.case = TRUE))

# Merge SCC data subset with NEI data
plot6sccunique <- merge(plot6scc, nei)

# Subset for Baltimore and Los Angeles county data
plot6sccunique <- plot6sccunique[plot6sccunique$fips == c("24510","06037"), ]

# Keep columns with emissions, year, and county data
plot6sccunique <- plot6sccunique[,c("Emissions", "year", "fips")]

# Group data and sum emissions by year, by county
plot6sccunique <- group_by(plot6sccunique, year, fips)
plot6sccunique <- plot6sccunique %>% summarise_each(funs(sum))

# Use ggplot2 to plot emissions in Baltimore and Los Angeles county from motor vehicle sources by year
qplot(plot6sccunique$year, plot6sccunique$Emissions, group = plot6sccunique$fips, color = plot6sccunique$fips, ylab = "PM2.5 Emissions from motor veh. sources", xlab = "Year") + geom_point() + geom_line() + geom_smooth(method="lm", fill=NA, lwd = .5, linetype = "dotted") + ylim(0, 4000) + guides(color=guide_legend(title="US County")) + scale_color_manual(values = c("red", "blue"), labels = c("Los Angeles", "Baltimore")) + labs(title = "Total PM2.5 emissions from motor vehicle sources \n in Baltimore and Los Angeles from 1999–2008")

# Create PNG of plot
dev.copy(png, file = "plot6.png")
dev.off()
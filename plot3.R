# Set working directory to folder with data
setwd("expcourproj2")

# Read NEI and SCC data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Check if "dplyr" package is installed (install if not installed) and load package
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

# Subset NEI data by county code for Baltimore
neibalt <- nei[nei$fips == "24510", ]

# Sum total emissions by year while grouping by "type"
neibaltbytype <- neibalt[ ,c(4:6)]
neibaltbytype <- group_by(neibaltbytype, type, year)
neibaltbytype <- neibaltbytype %>% summarise_each(funs(sum))

# Load ggplot2 package
library(ggplot2)

# Plot emissions in Baltimore by type
qplot(neibaltbytype$year, neibaltbytype$Emissions, group = neibaltbytype$type, color = neibaltbytype$type, ylab = "PM2.5 Emissions", xlab = "Year") + geom_point() + geom_line() + geom_smooth(method="lm", fill=NA, lwd = .5, linetype = "dotted") + ylim(0, 2200) + guides(color=guide_legend(title="Type")) + labs(title = "Total PM2.5 emissions by type in Baltimore from 1999 to 2008")


# Create PNG of plot
dev.copy(png, file = "plot3.png")
dev.off()
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999-2008?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library("plyr")
library("dplyr")
library("ggplot2")
options(scipen=999) # disable scientific notation

# plot4

# fetch all SCC records with Short.Name Coal & Comb
CoalComb <- subset(SCC, EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal",
                                         "Fuel Comb - Electric Generation - Coal",
                                         "Fuel Comb - Industrial Boilers, ICEs - Coal"))

CoalComb1 <- subset(SCC, grepl("Comb", Short.Name) & grepl("Coal", Short.Name), ignore.case = TRUE)


nrow(CoalComb)
nrow(CoalComb1)

d1 <- setdiff(CoalComb, CoalComb1)
length(d1)
# The matches in CoalComb and CoalComb1 differ by 15 rows. I am not fully aware of why this is the case.
# However, I will use the union function to put the diff

# Above does not work correctly

d2 <- setdiff(CoalComb$SCC, CoalComb1$SCC)
length(d2)

# Given these differences I believe the best approach is to union the two sets
CoalCombCodes <- union(CoalComb$SCC, CoalComb1$SCC)
length(CoalCombCodes)

ccSubset <- subset(NEI, SCC %in% CoalCombCodes)

ccSubsetPM25ByYear <- ddply(ccSubset, .(year, type), function(x) sum(x$Emissions))
colnames(ccSubsetPM25ByYear)[3] <- "Emissions"

png("plot4.png")
qplot(year, Emissions, data = ccSubsetPM25ByYear, color = type, geom="line") +
        stat_summary(fun.y = "sum", fun.ymin = "sum", fun.ymax = "sum", 
                     color = "blue", aes(shape="total"), geom="line") +
        geom_line(aes(size="total", shape = NA)) +
        ggtitle(expression("Coal Combustion Emmisions" ~ PM[2.5] ~ "by Source Type and Year")) +
        xlab("Year") +
        ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()

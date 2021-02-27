## Project 2 

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

setwd("C:/Users/Hriday/Documents/R/win-library/4.0/Data_Science_R_Coursera/Reproducible_Research/project2/Reproducible-Research-Project-2")

urldata <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(urldata,destfile = "stormdata.csv.bz2")

# Use cache over here in RMarkdown, DONT RE RUN!! IT TAKES A WHILE
stormdata <- read.csv("stormdata.csv.bz2",header = TRUE,stringsAsFactors = FALSE)

stormdata$year <- as.numeric(format(as.Date(stormdata$BGN_DATE,format = "%m/%d/%Y %H:%M:%S"),"%Y"))

# we are going to check which years have had more storms, in order to subset those and focus our analysis over there
hist_plot <- ggplot(stormdata,aes(x=year)) + geom_histogram(bins = 30,aes(y=..count..,fill=..count.. )) + 
    labs(title = "Storms per year",x="Year",y = "Frequency") + theme(legend.position = "none")
hist_plot
#hist(stormdata$year,breaks = 30,main="Storms per year",xlab = "Year",ylab = "Frequency",col = "red")

# We see that since nearly 1995 there are more storms, also, the data is more complete in those years. So 
# I will subset the data from 1995 until 2011, the latest data available.

subset_data <- stormdata[stormdata$year >= 1995,]
subset_data <- subset_data[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP",
                               "CROPDMG","CROPDMGEXP","year")]
dim(subset_data) 
# Now we have about 220,000 rows less


subset_data$EVTYPE <- toupper(subset_data$EVTYPE)

# Now I gotta use grepl to get only the 48 EVTYPE that are accepted. Could not get it done, not gonna subset that
eventnames <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill",
                "Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat",
                "Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze","Funnel Cloud","Freezing Fog","Hail",
                "Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane (Typhoon)","Ice Storm",
                "Lake-Effect Snow","Lakeshore Flood","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind",
                "Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide","Strong Wind",
                "Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash",
                "Waterspout","Wildfire","Winter Storm","Winter Weather")

eventnames <- toupper(eventnames)
subset_data <- subset_data[subset_data$EVTYPE %in% eventnames,]

## First question
harmful_events <- aggregate(x = data.frame(subset_data$FATALITIES,subset_data$INJURIES),by = list(subset_data$EVTYPE), FUN = "sum")
names(harmful_events) <- c("EVTYPE","FATALITIES","INJURIES")
harmful_events <- harmful_events[harmful_events$FATALITIES > 10 | harmful_events$INJURIES > 10,]

order_data <- harmful_events
order_data$fat_inj <- order_data$FATALITIES + order_data$INJURIES
order_data <- order_data[order(order_data$fat_inj,decreasing = TRUE),] 
order_data <- order_data[1:10,]

q1_plot_fat <- ggplot(order_data, aes(x = reorder(factor(EVTYPE),-FATALITIES), y = FATALITIES)) + 
    geom_bar(stat = "identity",aes(fill= FATALITIES)) + theme(legend.position = "none") + 
    labs(title = "Health Harmful Events: Fatalities", x = "Event Type",
        y = "Total Fatalities") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
q1_plot_inj <- ggplot(order_data, aes(x = reorder(factor(EVTYPE),-INJURIES), y = INJURIES)) + 
    geom_bar(stat = "identity",aes(fill = INJURIES)) + theme(legend.position = "none") +
        labs(title = "Health Harmful Events: Injuries", x = "Event Type",
         y = "Total Injuries") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(q1_plot_fat,q1_plot_inj,ncol=2)

# SECOND QUESTION 
# Work the property and crop damage, then aggregate the sum by event and do the same as before
subset_data$PROPDMGEXP <- as.character(subset_data$PROPDMGEXP)
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("0","1","2","3","4","5","6","7","8")] <- 10
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("?","-"," ","")] <- 0
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("+")] <- 1
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("H","h")] <- 100
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("K","k")] <- 1000
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("M","m")] <- 1000000
subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("B","b")] <- 1000000000

subset_data$PROPDMGEXP <- as.numeric(subset_data$PROPDMGEXP)

subset_data$TOTPROPDMG <- subset_data$PROPDMG * subset_data$PROPDMGEXP 

# Now for the crop
subset_data$CROPDMGEXP <- as.character(subset_data$CROPDMGEXP)
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("0","1","2","3","4","5","6","7","8")] <- 10
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("?","-"," ","")] <- 0
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("+")] <- 1
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("H","h")] <- 100
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("K","k")] <- 1000
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("M","m")] <- 1000000
subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("B","b")] <- 1000000000

subset_data$CROPDMGEXP <- as.numeric(subset_data$CROPDMGEXP)

subset_data$TOTCROPDMG <- subset_data$CROPDMG * subset_data$CROPDMGEXP 

subset_data$TOTDMG <- subset_data$TOTPROPDMG + subset_data$TOTCROPDMG

econ_dmg <- aggregate(subset_data$TOTDMG,list(subset_data$EVTYPE),FUN = "sum")
names(econ_dmg) <- c("EVTYPE","TOTDMG")

econ_dmg <- econ_dmg[econ_dmg$TOTDMG > 0,]
order_econ_dmg <- econ_dmg[order(econ_dmg$TOTDMG,decreasing = TRUE),]
order_econ_dmg <- order_econ_dmg[1:10,]

#TAKE OUT THE SCIENTIFIC NOTATION
q2_plot <- ggplot(order_econ_dmg, aes(x= reorder(factor(EVTYPE),-TOTDMG), y = as.numeric(TOTDMG))) + 
    geom_bar(stat="identity",aes(fill= TOTDMG)) +theme(legend.position = "none") +
    labs(title = "The 10 Events with the biggest Economic Consequences", x = "Event Type", y = "Total Economic Damage") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
q2_plot

#plot for conclusion
grid.arrange(q1_plot,q2_plot,ncol=2)

## I do not care for what happens at every state, I just care for the general data

# QUESTIONS:
## Across the United States, which types of events (as indicated in the EVTYPE variable) are most 
# harmful with respect to population health?
## Across the United States, which types of events have the greatest economic consequences?
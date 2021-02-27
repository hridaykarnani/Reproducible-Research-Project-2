# 1 Synopsis

Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern. This project
involves exploring the U.S. National Oceanic and Atmospheric
Administration’s (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage. The two main questions to
answer are: Across the United States, which types of events (as
indicated in the EVTYPE variable) are most harmful with respect to
population health? Across the United States, which types of events have
the greatest economic consequences?

First we will briefly process the data. The dataset contains almost a
million rows starting in 1950 until 2011. So we will need to subset it,
using only the important data. The main results obtained from the
analysis of the data are presented in the section “Results”.

# 2 Data Processing

First we will start by reading the data. The dataset can be found in the
following link:
[Dataset](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).
Also, we will load the libraries that will be used. The libraries used
are: ggplot2, dplyr,grid,gridExtra. Finally, let’s watch at the
dimmension of the data.

    stormdata <- read.csv("stormdata.csv.bz2",header = TRUE,stringsAsFactors = FALSE)
    dim(stormdata)

    ## [1] 902297     37

    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(grid)
    library(gridExtra)

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

Before doing anything, we are going to check a simple histogram of the
data present by year. We are going to check which years have had more
storms, in order to subset those and focus our analysis over there.
Since there is a lot of data, it is important to subset and reduce the
amount of rows, if possible.

    # First we create a new column that contains the year in which the event occured
    stormdata$year <- as.numeric(format(as.Date(stormdata$BGN_DATE,format = "%m/%d/%Y %H:%M:%S"),"%Y"))
    hist_plot <- ggplot(stormdata,aes(x=year)) + geom_histogram(bins = 30,aes(y=..count..,fill=..count.. )) + 
        labs(title = "Storms per year",x="Year",y = "Frequency") + theme(legend.position = "none")
    hist_plot

![](ProjectPresentation_files/figure-markdown_strict/storms%20histogram-1.png)

We see that since nearly 1995 there are more storms, also, the data is
more complete in those years (i.e, less missing values). So I will
subset the data from 1995 until 2011, the latest data available. Also,
we will subset the data only with the important columns. Those are the
columns that contain relevant information about the type of the event,
and their health and economic consequences.

    subset_data <- stormdata[stormdata$year >= 1995,]
    subset_data <- subset_data[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP",
                                   "CROPDMG","CROPDMGEXP","year")]
    dim(subset_data) 

    ## [1] 681500      9

Now we have about 220,000 rows less.

Next, we will try and clean a bit the data. “EVTYPE” variable means
“Event type”, you can notice (I will not do it because the result is to
extense) that there are to many different Event types. In the following
[PDF](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
you can find out that only 48 of them are considered “officials” in the
dataset there are a lot of mistakes (such as typos or differences
between Upper case and lower case words). Considering that, we will
create a character list that contains only the 48 official events and
subset our data to the rows that contain those events. Also, we will
make all the words Upper case, to avoid on more problem.

    subset_data$EVTYPE <- toupper(subset_data$EVTYPE)
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
    dim(subset_data)

    ## [1] 519244      9

Now we have **almost 400,000 (in total) rows less!**

In the next subsection, we will try to process the data and leave it
ready to answer the questions raised in the synopsis.

## 2.1 Data Processing: Which types of events are the most harmful with respect to population health?

In this subsection we will try to use the data in order to response the
question raised. First a new variable called “harmful\_events” will be
created. This variable contains the total amount of “fatalities” and
“injuries” that each type of event has caused. We will consider only
those events that have at least 10 fatalities or injuries in total in
the years selected, since a lot of them do not have big consequences.

    harmful_events <- aggregate(x = data.frame(subset_data$FATALITIES,subset_data$INJURIES),by = list(subset_data$EVTYPE), FUN = "sum")
    names(harmful_events) <- c("EVTYPE","FATALITIES","INJURIES")
    harmful_events <- harmful_events[harmful_events$FATALITIES > 10 | harmful_events$INJURIES > 10,]
    head(harmful_events)

    ##             EVTYPE FATALITIES INJURIES
    ## 2        AVALANCHE        223      159
    ## 3         BLIZZARD         71      385
    ## 5  COLD/WIND CHILL         95       12
    ## 6        DENSE FOG         10      276
    ## 9       DUST DEVIL          2       43
    ## 10      DUST STORM         21      420

    dim(harmful_events)

    ## [1] 31  3

We can see that only 31 (of the 48 type of events) have at least 10
fatalities or injuries. Next, we will order the data and subset it to
only the 10 events with more consequences. This will be stored in
“order\_data”.

    order_data <- harmful_events
    order_data$fat_inj <- order_data$FATALITIES + order_data$INJURIES
    order_data <- order_data[order(order_data$fat_inj,decreasing = TRUE),] 
    order_data <- order_data[1:10,]

With this data, we are ready to plot and answer the question raised
above, but we will do that in the “Result” section.

## 2.2 Data Processing: Which types of events have the greatest economic consequences?

In this subsection, we will try to use the data in order to response the
second question raised. To measure the economic consequences, we can use
the variables “PROPDMG” (Property Damage),“PROPDMGEXP”(Property Damage
Exponent),“CROPDMG” (Crop Damage) and “CROPDMGEXP” (Crop Damage
Exponent). The Property and Crop Damage Exponents are the exponents for
the Property and Crop Damage Exponents, but the values of the exponents
are not always intuitive, as we can see now:

    unique(subset_data$PROPDMGEXP)

    ##  [1] ""  "K" "M" "0" "5" "m" "2" "4" "+" "7" "-" "?" "3" "1" "8" "H" "B"

As we can see, we have to work out this data to analyze it. The
following lines of code will transform the Exponents variable in a
numeric one. This transformation is based in an article about [How to
handle Exponent
Value](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html)

    # First the Property Damage Exponent
    subset_data$PROPDMGEXP <- as.character(subset_data$PROPDMGEXP)
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("0","1","2","3","4","5","6","7","8")] <- 10
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("?","-"," ","")] <- 0
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("+")] <- 1
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("H","h")] <- 100
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("K","k")] <- 1000
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("M","m")] <- 1000000
    subset_data$PROPDMGEXP[subset_data$PROPDMGEXP %in% c("B","b")] <- 1000000000
    subset_data$PROPDMGEXP <- as.numeric(subset_data$PROPDMGEXP)
    #Now the Crop Damage Exponent
    subset_data$CROPDMGEXP <- as.character(subset_data$CROPDMGEXP)
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("0","1","2","3","4","5","6","7","8")] <- 10
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("?","-"," ","")] <- 0
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("+")] <- 1
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("H","h")] <- 100
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("K","k")] <- 1000
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("M","m")] <- 1000000
    subset_data$CROPDMGEXP[subset_data$CROPDMGEXP %in% c("B","b")] <- 1000000000
    subset_data$CROPDMGEXP <- as.numeric(subset_data$CROPDMGEXP)

Now, we can create a new variable that contains the total damage in
property and crop, “TOTDMGPROP” & “TOTDMGCROP”. This is simply the
multiplication between the Exponent and the damage value. We will also
create a total damage variable, that is just the addition of the total
property and crop damage.

    subset_data$TOTPROPDMG <- subset_data$PROPDMG * subset_data$PROPDMGEXP 
    subset_data$TOTCROPDMG <- subset_data$CROPDMG * subset_data$CROPDMGEXP 
    subset_data$TOTDMG <- subset_data$TOTPROPDMG + subset_data$TOTCROPDMG

With this done, we have a variable with an estimation of the total
economic consequences of each event.

Next, we will create a new variable called “econ\_dmg” that sums up the
economic damage of each type of event. This will simply sum up the
economic damage of each event.

    econ_dmg <- aggregate(subset_data$TOTDMG,list(subset_data$EVTYPE),FUN = "sum")
    names(econ_dmg) <- c("EVTYPE","TOTDMG")

We will filter this variable only to those event types that have an
economic damage bigger than 0. Next, we order the data decreasingly (i.e
the event with the highest economic damage appears first), this will be
done to filter only the 10 events with the highest economic damage.

    econ_dmg <- econ_dmg[econ_dmg$TOTDMG > 0,]
    order_econ_dmg <- econ_dmg[order(econ_dmg$TOTDMG,decreasing = TRUE),]
    order_econ_dmg <- order_econ_dmg[1:10,]

Now we have finished processing the data and we are ready to see some
results.

# 3 Results

To respond the first question: Which types of events are the most
harmful with respect to population health? We will plot the results of
what we did in the section 2.1. Separating the health consequences by
fatalities and injuries.

    q1_plot_fat <- ggplot(order_data, aes(x = reorder(factor(EVTYPE),-FATALITIES), y = FATALITIES)) + 
        geom_bar(stat = "identity",aes(fill= FATALITIES)) + theme(legend.position = "none") + 
        labs(title = "Health Harmful Events: Fatalities", x = "Event Type",
            y = "Total Fatalities") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    q1_plot_inj <- ggplot(order_data, aes(x = reorder(factor(EVTYPE),-INJURIES), y = INJURIES)) + 
        geom_bar(stat = "identity",aes(fill = INJURIES)) + theme(legend.position = "none") +
            labs(title = "Health Harmful Events: Injuries", x = "Event Type",
             y = "Total Injuries") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    grid.arrange(q1_plot_fat,q1_plot_inj,ncol=2)

![](ProjectPresentation_files/figure-markdown_strict/plot%20q1-1.png)

From this plot, we can conclude that Excessive Heat, Tornados and Floods
are among the most harmful events. Although Tornados generate a really
big amount of injuries.

To respond the second question: Which types of events have the greatest
economic consequences? We will plot the results of what we did in the
section 2.2. Using the total economic damage column as the relevant
variable to answer the question.

    q2_plot <- ggplot(order_econ_dmg, aes(x= reorder(factor(EVTYPE),-TOTDMG), y = as.numeric(TOTDMG))) + 
        geom_bar(stat="identity",aes(fill= TOTDMG)) +theme(legend.position = "none") +
        labs(title = "The 10 Events with the biggest Economic Consequences", x = "Event Type", y = "Total Economic Damage") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    q2_plot

![](ProjectPresentation_files/figure-markdown_strict/plot%20q2-1.png)

In the plot we can see that by large amounts, Floods have the highest
economic consequences, followed by Tornados and Hails.

# 4 Conclusion

In conclusion, watching at both figures we can notice that Tornados are
events that leave awful results, both in economics and health. Floods
are by far the events with the highest economic consequences, while
Tornados, Floods and Excessive Heat stand out in the health sector. It
is important to remember that we are using historic data since 1995
until 2011, years that contain a high presence of data (almost 520,000
events).

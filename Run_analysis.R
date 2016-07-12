## Run Analysis
## 
## Download file
if (!file.exists("./data/archive.zip"))
        {
                file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
                file.destination <- "data"
                file.name <- "storms.csv"
                date.downloaded <- Sys.Date()
                download.file(file.url,"./data/archive.zip","curl",quiet=TRUE)
                ##unzip("./data/archive.zip",exdir="./data")
        }
## read the data into R
dfstorms <- read.csv("./data/archive.zip",stringsAsFactors = TRUE)
if(!"storms" %in% ls()){
dfstorms <- read.csv("./data/archive.zip",stringsAsFactors = TRUE)
## Making the dataframe as table as they are easier to print and manipulate
}
library(dplyr)
storms <- tbl_df(dfstorms)
## removing the data fram object form memory (> 400MB)
#rm("dfstorms")


##Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variabl) are most harmful with respect to population health?
##Across the United States, which types of events have the greatest economic consequences?

## So we need some processing of our data
## first let's check how clean our data is

## TODO:
##      Normalize event types: trim and make case the same (upper case)
##      Turn date strings in date (using lubridate)
##      Extract what we need for this excersise, evttype, date, fatalities, injuries and research waht fields are related to economic impact
storms$EVTYPE <- toupper(trimws(storms$EVTYPE,which="both"))
library(lubridate)
storms$BGN_DATE <- mdy_hms(as.character(storms$BGN_DATE))
storms$EVTYPE <- as.factor(storms$EVTYPE)
##
storms.impact <- storms %>%
                        select(BGN_DATE,EVTYPE,STATE,FATALITIES,INJURIES,PROPDMG,CROPDMG)

storms.health.summary <- storms.impact %>%
                                group_by(
                                        EVTYPE,
                                        YEAR=year(BGN_DATE)) %>%
                                summarise(
                                        POPIMPACT=sum(FATALITIES + INJURIES)
                                        )
storms.cost.summary <- storms.impact %>%
                                group_by(
                                        EVTYPE,
                                        YEAR=year(BGN_DATE)) %>%
                                summarise(
                                        COSTIMPACT=sum(CROPDMG + PROPDMG)
                                )
##Breaks data by decade        
storms.health.summary <- mutate(
                                storms.health.summary,
                                DECADE=cut(YEAR,
                                           breaks=seq(1949,2020,by=10),
                                           labels=c("50s","60s","70s","80s","90s","00s","10s")
                                           )
                                )
## Looking at Population Health Impact
##r events that had 0 casualties 
storms.health.summary <- filter(storms.health.summary,
                                POPIMPACT !=0 
                                )
## looked at most impactful events by decade on heatlh
storms.health.summary.by.decade <- storms.health.summary %>%
                         group_by(
                                  DECADE,
                                  EVTYPE) %>%
                         summarise(
                                 POPIMPACT2 = sum(POPIMPACT)
                                 )
## find top 3 impact by decades
top.impact.by.decade <- arrange(
        top_n(
                storms.health.summary.by.decade,3
        ),
        DECADE,
        desc(POPIMPACT2),
        EVTYPE)
## Plot it
library(ggplot2)
myp <- ggplot(top.impact.by.decade,aes(DECADE,POPIMPACT2,fill=EVTYPE)) 
myp <- myp + geom_bar(stat="identity") 
myp <- myp   
print(myp)
## Overall since 80s
storms.health.summary.overall <- storms.health.summary %>%
                                filter(
                                        DECADE %in% c("80s","90s","00s","10s")
                                        ) %>%
                                group_by(EVTYPE) %>%
                                summarise(
                                        POPIMPACT2 = sum(POPIMPACT)
                                ) %>%
                                arrange(desc(POPIMPACT2)) %>%
                                mutate(RANK=cume_dist(POPIMPACT2)) %>%
                                filter(RANK > 0.98)

                                

## Plot it
myp <- ggplot(data=storms.health.summary.overall,
                aes(
                        reorder(EVTYPE,-POPIMPACT2),
                        POPIMPACT2
                        )
                )
myp <- myp + geom_bar(stat="identity",fill="lightblue",width=0.2) + guides(fill=FALSE)
myp <- myp + ggtitle("Top 5 Weather Event as Population Health Impact")
myp <- myp + xlab("Weather Event Type") + ylab("Casualties/Injuries")

## fixing titles and legends
print(myp)
### Now looking at costs impact##
### 
##Breaks data by decade        
storms.costs.summary <- mutate(
        storms.impact,
        DECADE=cut(year(BGN_DATE),
                   breaks=seq(1949,2020,by=10),
                   labels=c("50s","60s","70s","80s","90s","00s","10s")
        ),
        COSTIMPACT = PROPDMG+CROPDMG
)
## Filte# Looking at Population costs Impact
##r events that had 0 casualties 
storms.costs.summary <- filter(storms.costs.summary,
                                COSTIMPACT !=0 
                        )
## looked at most impactful events by decade on heatlh
storms.costs.summary.by.decade <- storms.costs.summary %>%
        group_by(
                DECADE,
                EVTYPE) %>%
        summarise(
                COSTIMPACT2 = sum(COSTIMPACT)
        )
## Overall since 80s
storms.costs.summary.overall <- storms.costs.summary %>%
        filter(
                DECADE %in% c("80s","90s","00s","10s")
        ) %>%
        group_by(EVTYPE) %>%
        summarise(
                COSTIMPACT2 = sum(COSTIMPACT)
        ) %>%
        arrange(
                desc(COSTIMPACT2)
                ) %>%
        top_n(5)
storms.costs.summary.overall <- arrange(storms.costs.summary.overall,desc(COSTIMPACT2))
## find top 3 impact by decades
top.costs.by.decade <- arrange(
        top_n(
                storms.costs.summary.by.decade,3
        ),
        DECADE,
        desc(COSTIMPACT2),
        EVTYPE)

## Plot it
## Little hack to order the EVTYPE appropriately, ordering the factors
library(ggplot2)
myp <- ggplot(storms.costs.summary.overall,aes(x=reorder(EVTYPE,-COSTIMPACT2),COSTIMPACT2/1e+6) )
myp <- myp + geom_bar(stat="identity",fill="lightblue",width=0.2) + guides(fill=FALSE)
myp <- myp + ggtitle("Top 5 Weather Event as Economic Impact")
myp <- myp + xlab("Weather Event Type") + ylab("Crop and Property Damages in Million USD")
print(myp)














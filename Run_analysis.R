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
## Making the dataframe as table as they are easier to print and manipulate
library(dplyr)
storms <- tbl_df(dfstorms)
## removing the data fram object form memory (> 400MB)
rm("dfstorms")

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
storms$BGN_DATE <- mdy_hms(storms)
storms$EVTYPE <- as.factor(storms$EVTYPE)
##
storms.impact <- storms %>%
                        select(BGN_DATE,EVTYPE,STATE,FATALITIES,INJURIES,PROPDMG,CROPDMG)

storms.health.summary <- storms.impact %>%
                                group_by(EVTYPE,YEAR=year(BGN_DATE)) %>%
                                summarise(POPIMPACT=sum(FATALITIES + INJURIES))
##Breaks data by decade        
storms.health.summary <- mutate(
                                storms.health.summary,
                                DECADE=cut(YEAR,
                                           breaks=seq(1949,2020,by=10),
                                           labels=c("50s","60s","70s","80s","90s","00s","10s")
                                           )
                                )
## Filter events that had 0 casualties
storms.health.summary <- filter(storms.health.summary,
                                POPIMPACT !=0
                                )
## looked at most impactful events by decade
storms.health.summary.by.decade <- storms.health.summary %>%
                         group_by(
                                  DECADE,
                                  EVTYPE) %>%
                         summarise(
                                 POPIMPACT2 = sum(POPIMPACT)
                                 )
## Plot it
myp <- ggplot(storms.health.summary.by.decade,aes(DECADE,POPIMPACT2,fill=EVTYPE)) 
myp <- myp + geom_bar(stat="identity") 
myp <- myp + guides(fill=FALSE)  
print(myp)























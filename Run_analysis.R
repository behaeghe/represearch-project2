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
evtype <- unique(storms$EVTYPE)
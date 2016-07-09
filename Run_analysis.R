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

dfstorms <- read.csv("./data/archive.zip",stringsAsFactors = FALSE)

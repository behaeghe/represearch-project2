## ################################
## Author: JRB
## Date: July 8th 2016
## Version: 0.1
## Function: file.download.unzip
## Arguments:
##      file.url: url of the file to download
##      file.destination: destination directory of the file
##  Description:
##      Function will download file specified in file.url into file.destination 
##      folder will create the folder if needed, unpack if the file is an archive.
##      function return the date file was downloaded 
file.download.unzip <- function (file.url,file.destination,unpack=TRUE) {
        
#file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#file.destination <- "data"
#file.name <- "storms.csv"
archive.name <- "temp.zip"
full.path <- file.path(file.destination,file.url)
if (!dir.exists(file.destination)) 
{dir.create(file.path(file.destination)}
try(download.file(file.url,, "curl"))
if (!file.exists){}
return(Sys.Date())
}
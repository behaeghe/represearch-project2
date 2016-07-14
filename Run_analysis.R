## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(tidy=TRUE)
knitr::opts_chunk$set(message=FALSE)

## ----aquiring data, cache=TRUE,include=TRUE------------------------------
if (!file.exists("./data/archive.zip"))
        {
                file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
                file.destination <- "data"
                file.name <- "storms.csv"
                date.downloaded <- Sys.Date()
                download.file(file.url,"./data/archive.zip","curl",quiet=TRUE)
        }
## read the data into R
dfstorms <- read.csv("./data/archive.zip",stringsAsFactors = TRUE)

## Making the dataframe as table as they are easier to print and manipulate
library(dplyr)
storms <- tbl_df(dfstorms)
str(storms)

## ----data cleaning, cache=TRUE, include=TRUE-----------------------------
##Cleaning up the EVTYPE, through exploration of the data noticed some inconsitencies so 
##addressing those here

## Inconsistencies of reporting Thunderstorm wind as TSTM WInd and Thunderstorm Wind
## this line addresses the issue and makes it conistent as "THUNDERSTORM WINDS"
library(lubridate)
storms <- storms %>% 
                mutate(
                        EVTYPE = toupper(trimws(storms$EVTYPE,which="both")),
                        BGN_DATE = mdy_hms(as.character(storms$BGN_DATE))
                      ) %>%
                mutate(
                        EVTYPE = gsub("^TSTM WIND","THUNDERSTORM WIND",EVTYPE)
                ) %>%
                mutate(
                       EVTYPE = gsub("^THUNDERSTORM WIND","THUNDERSTORM WIND",EVTYPE)
                       ) %>%
                mutate(
                        EVTYPE = gsub("^THUNDERSTORM WINDS","THUNDERSTORM WIND",EVTYPE)
                ) %>%
                mutate(
                        CROPDMGEXP = toupper(CROPDMGEXP),
                        PROPDMGEXP = toupper(PROPDMGEXP)
                )%>%
                mutate(
                        EVTYPE = as.factor(EVTYPE)
                )
                 

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## ----data analysis, include=TRUE-----------------------------------------
library(dplyr,quietly=TRUE)
library(lubridate,quietly = TRUE)
storms.impact <- storms %>%
                        select(BGN_DATE,
                               EVTYPE,
                               STATE,
                               FATALITIES,
                               INJURIES,
                               PROPDMG,
                               PROPDMGEXP,
                               CROPDMG,
                               CROPDMGEXP)

## ----population impact, include=TRUE-------------------------------------
## storms.health.summary aggregate the sum of fatalities and injuries by year and event type
storms.health.summary <- storms.impact %>%
                                group_by(
                                        EVTYPE,
                                        YEAR=year(BGN_DATE)) %>%
                                summarise(
                                        POPIMPACT=sum(FATALITIES + INJURIES)
                                        )
##The summary data is broken down by decade for results discussion       
storms.health.summary <- mutate(
                                storms.health.summary,
                                DECADE=cut(YEAR,
                                           breaks=seq(1949,2020,by=10),
                                           labels=c("50s","60s","70s","80s","90s","00s","10s")
                                           )
                                )
## Only events that had casualties are being considered
storms.health.summary <- filter(storms.health.summary,
                                POPIMPACT !=0 
                                )
## Aggregate population health data by decade for future plotting
storms.health.summary.by.decade <- storms.health.summary %>%
                         group_by(
                                  DECADE,
                                  EVTYPE) %>%
                         summarise(
                                 POPIMPACT2 = sum(POPIMPACT)
                                 )
## Define the top 3 impact by decades for plotting
top.impact.by.decade <- arrange(
        top_n(
                storms.health.summary.by.decade,3
        ),
        DECADE,
        desc(POPIMPACT2),
        EVTYPE)
## Aggregate Population Heatlh Impact since 1950 and select the 98% percentile and above
storms.health.summary.overall <- storms.health.summary %>%
                                filter(
                                        DECADE %in% c("50s","60s","70s","80s","90s","00s","10s")
                                        ) %>%
                                group_by(EVTYPE) %>%
                                summarise(
                                        POPIMPACT2 = sum(POPIMPACT)
                                ) %>%
                                arrange(desc(POPIMPACT2)) %>%
                                mutate(RANK=cume_dist(POPIMPACT2)) %>%
                                filter(RANK > 0.98)


## ----costs impact, include=TRUE------------------------------------------
storms.cost.summary <- storms.impact %>%
                                group_by(
                                        EVTYPE,
                                        YEAR=year(BGN_DATE)) %>%
                                summarise(
                                        COSTIMPACT=sum(CROPDMG + PROPDMG)
                                )
storms.cost.summary <- storms.impact %>% 
                                mutate(
                                        CROPDMGEXP = gsub("[kK]","1e+3",CROPDMGEXP)
                                #         )## %>%
                                # mutate(
                                #        CROPDMGEXP = gsub("[mM]","1e+6",CROPDMGEXP) 
                                # ) %>%
                                # mutate (
                                #         CROPDMGEXP = gsub("[bB]","1e+9",CROPDMGEXP)
                                # )
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
                DECADE %in% c("70s","80s","90s","00s","10s")
        ) %>%
        group_by(EVTYPE) %>%
        summarise(
                COSTIMPACT2 = sum(COSTIMPACT)
        ) %>%
        arrange(
                desc(COSTIMPACT2)
                ) %>%
        mutate(RANK=cume_dist(COSTIMPACT2)) %>%
        filter(RANK > 0.98)
storms.costs.summary.overall <- arrange(storms.costs.summary.overall,desc(COSTIMPACT2))
## find top 3 impact by decades
top.costs.by.decade <- arrange(
        top_n(
                storms.costs.summary.by.decade,3
        ),
        DECADE,
        desc(COSTIMPACT2),
        EVTYPE)


## ----Population harm by weather event and decades (1950-2011), include=TRUE,fig.pos="H"----
library(ggplot2)
myp <- ggplot(top.impact.by.decade,aes(DECADE,POPIMPACT2,fill=EVTYPE)) 
myp <- myp + geom_bar(stat="identity",width=0.5) 
myp <- myp  
myp <- myp + labs(title="Most Harmful Weather Event on \n Population Health by Decades \n (Top 3 by Decade)",y="Casualties/Injuries",x="Decade")
myp <- myp + scale_fill_brewer(palette="Spectral", name="Weather Event")
myp <- myp + theme_bw() 
print(myp)

## ----Cleveland Plots, include=TRUE,fig.pos="H",fig.width=9---------------
## Cleveland Dot Plot
## 
cutoff <- 10
cleveland.costs <- top_n(aggregate(data=storms.cost.summary, COSTIMPACT ~ EVTYPE,sum),cutoff)

myp1 <- ggplot(cleveland.costs,
               aes(x=COSTIMPACT/1e+6,
                   y=reorder(EVTYPE,COSTIMPACT)
                   )
        )

myp1 <- myp1 + geom_point(size=3,col="grey30")
myp1 <- myp1 + theme_bw()
myp1 <- myp1 + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y=element_line(colour="grey60",linetype="dashed"
                                                     ))
myp1 <- myp1 + ggtitle("Cost Impact of \n Weather Event") 
myp1 <- myp1 + xlab("Costs in Million USD")
myp1 <- myp1 + theme(axis.title.y=element_blank())


##
##
cleveland.costs <- top_n(aggregate(data=storms.health.summary, POPIMPACT ~ EVTYPE,sum),cutoff)
myp2 <- ggplot(cleveland.costs,
               aes(x=POPIMPACT,
                   y=reorder(EVTYPE,POPIMPACT)
               )
)
myp2 <- myp2 + geom_point(size=3,col="grey30")
myp2 <- myp2 + theme_bw()
myp2 <- myp2 + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y=element_line(colour="grey60",linetype="dashed"
                     ))
myp2 <- myp2 + ggtitle("Population Harm of \n Weather Event") 
myp2 <- myp2 + xlab("Casualties and Injuries (log)")
myp2 <- myp2 + theme(axis.title.y=element_blank())
myp2 <- myp2 + scale_x_log10()

multiplot(myp1,myp2,cols=2)


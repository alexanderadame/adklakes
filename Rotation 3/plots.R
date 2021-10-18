rm(list = ls(all.names = TRUE))
library(tidyverse)
library(adklakedata)
library(ggplot2)
library(maps)
library(rgdal)
library(ggsn)
library(ggExtra)
library(ggmap)
library(gridExtra)
library(grid)
library(magick)
library(ggrepel)
library(reshape2)
library(lubridate)
library(openair)
library(plyr)
library(dplyr)
library(purrr)
library(rlist)
library(trend)
options(max.print = 1000000)


########## Read in data and add date columns
fullmerge <- read.csv("ALTM_1992_2020.csv")
fullmerge$Sample.Date.MM.DD.YYYY <- mdy(fullmerge$Sample.Date.MM.DD.YYYY)
fullmerge$Sample.Date.MM.DD.YYYY<- as.Date(fullmerge$Sample.Date.MM.DD.YYYY)
fullmerge$Sample.Date.MM.DD.YYYY <- ymd(fullmerge$Sample.Date.MM.DD.YYYY)
fullmerge$Year <- year(fullmerge$Sample.Date.MM.DD.YYYY)
fullmerge$month <- month(fullmerge$Sample.Date.MM.DD.YYYY, label = TRUE)
write_excel_csv(fullmerge, "fullmerge.csv")                                        
                                        
fullmerge <- fullmerge[order(fullmerge$lake.name,fullmerge$Year), ]
                                        
to2017 <- fullmerge[fullmerge$Year <= 2017, ]
from2017 <- fullmerge[fullmerge$Year >= 2018, ]
                                        
################### Pull data by variable and calculate averages
lakesum <- fullmerge %>%
  group_by(lake.name, month, DOC.mg.L.1, TRUCOLOR.PtCo, LABPH, ANC.Ã.Âµeq.L.1)
test <- aggregate(lakesum$DOC.mg.L.1, by=list(lakesum$lake.name, lakesum$month, lakesum$Year), FUN=sum)
test$monmean <- aggregate(lakesum$DOC.mg.L.1, by=list(lakesum$lake.name,lakesum$month, lakesum$Year), FUN=length)
test$MonMean <- test$x / test$monmean[4]
                                        
lakeMonAvg <- as.data.frame(test)
lakeMonAvg$lake.name <- lakeMonAvg$Group.1
lakeMonAvg$month <- lakeMonAvg$Group.2
lakeMonAvg$year <- lakeMonAvg$Group.3
lakeMonAvg$monthly.average <- lakeMonAvg$MonMean$x
lakeMonAvg <- lakeMonAvg[7:10]

#write_excel_csv(lakeMonAvg, "lakeMonAvg.csv")

###################
##
## PLOT DOC MONTHLY AVERAGE FOR ALL LAKES

pdf("sens.pdf", onefile = TRUE)
for (i in 1:56)
{
  df1 = as.data.frame(splitsens[i])
  plotdf1 <- ggplot(df1, aes_string(x = names(df1[2]), y = names(df1[4]))) + geom_smooth() +
    theme_classic() + labs(title = names(splitsens[i]))
  print(plotdf1)
}
dev.off()

##### Sen's Slope for every lake ###### 
lakeMonAvg <- read.csv("LakeMonAvg_DOC.csv")
lakenames <- as.data.frame(lakenames)
alldat <- data.frame()
lakeMonAvg <- lakeMonAvg %>%
  drop_na()

i = "ARBUTUS LAKE"
j = "Jan"
#select every lake every
for (i in unique(lakenames[1:60,1])) {
  print(i)
  #filter to get just one lake at a time
  one_lake <- filter(lakeMonAvg,ï..lake.name == i)
  #for every month for that lake
  for (j in unique(one_lake$month)) {
    print(j)
    #select one month
    one_month <- filter(one_lake, month == j)
    
    # run sen's slope
    # you can select valuesx from a list using the $ 
    # a quick way to do this is to open the list in the enviornment
    #click the drop down to get to the value you want then click the arror on the right side of the value
    #this will print a line to the console hit enter and you'll get that value
    # create sen's slope object
    ss <- sens.slope(one_month$monthly.average)
    # pull out the p value
    
    # you can pull out values from the list in the way I just described, or by calling the object and using the $ sometimes works.. best to view the list object 
    #I'm showing both ways here
    p_value <- ss$p.value
    # get the trend
    trend <- ss[["estimates"]][["Sen's slope"]]
    
    #need the lake and month again for the data frame
    lake <- i
    month <- j
    # create data frame 
    df <- data.frame(lake, month, p_value, trend)
    
    # rbind the data frame
    # Note you'll need to re run the alldat <- data.frame() line each time!
    alldat <- rbind(alldat, df)
  }
}

sensdf <- alldat
sensdf <- sensdf %>%
  group_by(sensdf$month)

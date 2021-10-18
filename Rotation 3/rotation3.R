#rm(list = ls(all.names = TRUE))
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
options(max.print = 1000000)


#
#altm <- as.data.frame(read.csv("ALTM_02_28_2020.csv", header = TRUE))


#
lakes = adk_lakes()
chem = adk_data('chem') 
crustacean = adk_data('crustacean') 
meta = adk_data('meta')
nutrient = adk_data('nutrient') 
phyto = adk_data('phyto') 
rotifer = adk_data('rotifer') 
secchi = adk_data('secchi') 
tempdo = adk_data('tempdo') 
met = adk_data('met') 

#chem adklakes plots 4/6/2020
#automate
library(purrr)
response = names(chem)[2]
expl = names(chem)[6:25]
response = set_names(response)
response
expl = set_names(expl)
expl


#
scatter_fun = function(x, y) {
  ggplot(chem, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() +
    labs(x = x,
         y = y)
}
#
scatter_box = function(x, y) {
  ggplot(chem, aes(x = .data[[x]], y = .data[[y]]), color = chem$year ) +
    geom_jitter(aes(color = year), height = .1, width = .35, size = 2) +
    geom_boxplot(outlier.colour="black", alpha = .4) +
    stat_boxplot(geom = 'errorbar', alpha = .3)
    theme_bw() +
    labs(x = x,
         y = y) +
    scale_colour_gradient(
    low = "#fadda2",
    high = "#0f3b59",
    space = "Lab",
    na.value = "grey50"
  )
}

elev_plots = purrr::map(expl, ~scatter_box(.x, "lake.name"))
elev_plots

#seperate by years
chem$date <- ydm(chem$date)
bpdataframe$Year <- as.Date(bpdataframe$Year, format = "%y")
to2012 <- selectByDate(chem, start = "1900-01-01", end = "2013-01-01", year = 1994:2012)
yrs <- c(from2017$Year)
bpdataframe$Year <- lubridate::ymd(yrs, truncated = 2L)
bpdataframe$Year <- ydm(bpdataframe$Year)
from2017$Year <- lubridate::ymd(yrs, truncated = 2L)
from2017 <- selectByDate(bpdataframe, start = "1900-01-01", end = "2020-01-01", year = 2012:2020)
from2017 <- bpdataframe[bpdataframe$Year >= 2013, ]
#
gg <- ggplot(from2017, aes(x = lake.name, y = DOC.mg.L.1)) +
  geom_jitter(height = .1, width = .35, size = 2, alpha = .8) +
  theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 11, face = "bold"), axis.title = element_text(size = 20)) +
  ggtitle("DOC in adklakes Dataset (new)") +
  xlab("Lake Name") +
  ylab("DOC in mg/L") +
  stat_boxplot(alpha= .2) +
  stat_boxplot(geom = 'errorbar', alpha = .2) +
  scale_colour_gradient(
    low = "#fadda2",
    high = "#0f3b59",
    space = "Lab",
    na.value = "grey50"
  )

gg2 <- ggplot(to2004, aes(x = lake.name, y = DOC, color = year)) +
  geom_jitter(aes(color = year), height = .1, width = .35, size = 2, alpha = .8) +
  theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 11, face = "bold"), axis.title = element_text(size = 20)) +
  ggtitle("DOC in adklakes Dataset (2004-2020)") +
  xlab("Lake Name") +
  ylab("DOC in mg/L") +
  stat_boxplot(alpha= .2) +
  stat_boxplot(geom = 'errorbar', alpha = .2) +
  scale_colour_gradient(
    low = "#ffc4cb",
    high = "#129638",
    space = "Lab",
    na.value = "grey50"
  )
plot(gg) + plot(gg2)


#new data cleanup 4/6/2020


bpdata <- as.data.frame(read.csv(file = "merged_data.csv", header= TRUE))
#doc <- subset(boxplotdata, select = c("lake.name", "Sample.Date.YYYY.MM.DD","DOC.mg.L.1"))
melt(bpdata, id=c('lake.name','Sample.Date.YYYY.MM.DD'))

#convert to date format
bpdata$Sample.Date.YYYY.MM.DD <- ymd(bpdata$Sample.Date.YYYY.MM.DD)
bpdata$Year <- year(bpdata$Sample.Date.YYYY.MM.DD)

bpdata$lake.name <- as.character(bpdata$lake.name)                   
bpdata$lake.name <- toupper(bpdata$lake.name)

#bpdata$YearMon <- as.Date(bpdata$YearMon, "%Y/%m")
bpdata2 <- select(bpdata, lake.name, DOC.mg.L.1, Year)
bpdata$DOC.mg.L.1 <- subset.data.frame(bpdata, bpdata$DOC.mg.L.1 < 60, select = DOC.mg.L.1,drop = TRUE)
bpdata2$lake.name <- gsub("-.*","", bpdata2$lake.name, ignore.case = TRUE, perl = TRUE)
bpdataframe <- bpdata2[order(bpdata2$lake.name,bpdata2$Year),]
# plot
bpdataframe$Year <- as.numeric(bpdata2$Year)

## convert and sort dates
bpdata$Year <- as.Date(bpdata$Year, format = "%y")
to2012 <- selectByDate(bpdata$Year, start = "1900-01-01", end = "2013-01-01", year = 1994:2012)
from2012 <- selectByDate(bpdata$Year, start = "1900-01-01", end = "2020-01-01", year = 2012:2020)
from2012 <- bpdataframe[bpdataframe$Year >= 2013, ]
library(RColorBrewer)


gg <- ggplot(fullmerge, aes(x = lake.name, y = DOC.mg.L.1, color = Year)) +
  geom_jitter(size = 4.8, alpha = 1.5) +
  theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 11, face = "bold"), axis.title = element_text(size = 20)) +
  ggtitle("DOC in Adirondack Water Systems Across 29 Years") +
  xlab("Lake Name") +
  ylab("DOC in mg/L") +
  stat_boxplot(alpha= .2) +
  stat_boxplot(geom = 'errorbar', alpha = .2) +
  scale_colour_gradient(
    low = "#fadda2",
    high = "#0f3b59",
    space = "Lab",
    na.value = "grey50"
  )

plot(gg)                                  
ggsave("DOC_29years.png", height = 9, width = 16)

write.csv(bpdata2, file ="bpdata.csv")

### Read in merged adirondack lake data 1992-2020

fullmerge <- read.csv("ALTM_1992_2020.csv")
### convert altm values
# altm <- as.data.frame(read.csv("ALTM_02_28_2020.csv", header = TRUE))
# altm$NH4..mg.L.1 <- altm$NH4..mg.L.1 * 0.0029890402
# altm$Ca2..mg.L.1 <- altm$Ca2..mg.L.1 * 0.0400769
# altm$Cl..mg.L.1 <- altm$Cl..mg.L.1 * 0.035453
# altm$Chlorophyll.a.µg.L.1 <- altm$Chlorophyll.a.µg.L.1 * .89349
# altm$DIC.mg.L.1 <- altm$DIC.mg.L.1 * 0.012011
# altm$DOC.mg.L.1 <-altm$DOC.mg.L.1 * 0.012011
# altm$Mg2..mg.L.1 <- altm$Mg2..mg.L.1 * 0.024305
# altm$NO3..mg.L.1 <- altm$NO3..mg.L.1 * 0.0620049
# altm$AL_OM.µg.L.1 <- altm$AL_OM.µg.L.1 * 0.026981539
# altm$AL_TD.µg.L.1 <- altm$AL_TD.µg.L.1 * 0.026981539
# altm$AL_TM.µg.L.1 <- altm$AL_TM.µg.L.1 * 0.026981539
# altm$K..mg.L.1 <- altm$K..mg.L.1 * 0.0390983
# altm$SiO2.mg.L.1 <- altm$SiO2.mg.L.1 * 0.06008
# altm$Na..mg.L.1 <- altm$Na..mg.L.1 * 0.022989769
# altm$SO42..mg.L.1 <- altm$SO42..mg.L.1 * 0.09606

######## Format fullmerge file #########
fullmerge$Sample.Date.YYYY.MM.DD <- mdy(fullmerge$Sample.Date.YYYY.MM.DD)
fullmerge$Sample.Date.YYYY.MM.DD <- as.Date(fullmerge$Sample.Date.YYYY.MM.DD, format = "%y"-"%m"-"%d")
fullmerge$Year <- year(fullmerge$Sample.Date.YYYY.MM.DD)
fullmerge$lake.name <- as.character(fullmerge$lake.name)                   
fullmerge$lake.name <- toupper(fullmerge$lake.name)
fullmerge$lake.name <- gsub("-.*","", fullmerge$lake.name, ignore.case = TRUE, perl = TRUE)
fullmerge <- fullmerge[order(fullmerge$lake.name,fullmerge$Year), ]
fullmerge <- filter(fullmerge, DOC.mg.L.1 != 113)

to2017 <- fullmerge[fullmerge$Year <= 2017, ]
from2017 <- fullmerge[fullmerge$Year >= 2018, ]

gg <- ggplot(from2017, aes(x = lake.name, y = DOC.mg.L.1)) +
  geom_boxplot(size = 1, alpha = .3, color = "blue", shape = "0", outlier.colour = "#12095e", outlier.shape = 18, outlier.alpha = .25, outlier.size = 3) +
  geom_boxplot(data = to2017, size = 1, alpha = .1, color = "orange", shape = "1", outlier.colour = "#b05b12", outlier.shape = 16, outlier.alpha = .25, outlier.size = 2) +
  ylim(0,25) +
  theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 9, face = "bold"), axis.title = element_text(size = 20)) +
  ggtitle("DOC - Orange is pre-switch - Blue is post ") +
  xlab("Lake Name") +
  ylab("DOC in mg/L") #+
  #stat_boxplot(alpha= .2) +
  #stat_boxplot(geom = 'errorbar', alpha = .2) +
  #scale_colour_gradient(
  #  low = "#fadda2",
  #  high = "#0f3b59",
  #  space = "Lab",
  #  na.value = "grey50"
  #)
gg

#automate all variable plots
library(purrr)
response = names(lakeMonAvg)[1]
expl = names(lakeMonAvg)[2:3]
response = set_names(response)
response
expl = set_names(expl)
expl


#
scatter_fun = function(x, y) {
  ggplot(lakeMonAvg, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = fullmerge$Year < 2017) +
    labs(x = x,
         y = y)
}
#
scatter_box = function(x, y) {
  ggplot(from2017, aes(x = .data[[y]], y = .data[[x]])) +
    geom_boxplot(size = 1, alpha = .3, color = "blue", shape = "0", outlier.colour = "#12095e", outlier.shape = 18, outlier.alpha = .25, outlier.size = 3) +
    geom_boxplot(data = to2017, size = 1, alpha = .1, color = "orange", shape = "1", outlier.colour = "#b05b12", outlier.shape = 16, outlier.alpha = .25, outlier.size = 2) +
    #ylim(-1, 5) +
    theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 9, face = "bold"), axis.title = element_text(size = 20))
   
}

elev_plots = purrr::map(expl, ~scatter_box(.x, "lake.name"))
pdf(file="monavg.pdf", width = 14, height = 10)
elev_plots[]
dev.off()

##########################count datapoints per lake.name

lake_counts <- count(fullmerge, lake.name)
write_excel_csv(lake_counts, "LakeCounts.csv")
fullmerge$month <- month(fullmerge$Sample.Date.YYYY.MM.DD, label = TRUE, )

########################## Nice looking boxplot
# ggsum <- ggplot(split_fullmerge$`ARBUTUS LAKE`, aes(month, DOC.mg.L.1, color = Year)) +
#   geom_jitter(height = .1, width = .35, size = 2, alpha = .8) +
#   theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 11, face = "bold"), axis.title = element_text(size = 20)) +
#   ggtitle("ARBUTUS LAKE") +
#   xlab("Month") +
#   ylab("DOC in mg/L") +
#   stat_boxplot(alpha= .2) +
#   stat_boxplot(geom = 'errorbar', alpha = .2) +
#   scale_colour_gradient(
#     low = "#fadda2",
#     high = "#0f3b59",
#     space = "Lab",
#     na.value = "grey50"
#   )
# ggsum

split <- split(lakesum, lakesum$lake.name)
split_fullmerge_test <- split(fullmerge, fullmerge$lake.name)

##
for (i in split_fullmerge) {
  elev_plots = purrr::map(expl, ~scatter_fun(.x, "lake.name"))
  pdf(file="monthPerLake.pdf", width = 14, height = 10)
  elev_plots[]
  dev.off()
}

lakenames <- unique(lakesum$lake.name)
split_fullmerge_test <- split_fullmerge
do.call("rbind", split_fullmerge_test)
unsplit <- unsplit(split_fullmerge_test, lakesum$lake.name)
Names = c("A","B","C")
for (i in 1:length(split_fullmerge)) {
  names(split_fullmerge[[i]])[names(split_fullmerge[[i]]) == "x"] = Names[i]
}

##
for (i in split_fullmerge) {
     aggregate(i[, 15], list(i$Year), mean)
}
## Create plots for DOC yearly average all lakes
for (i in split_fullmerge_test) {
  split_fullmerge_test$i <- split_fullmerge_test$i %>%
  group_by(i$Year) %>%
  summarise_at(vars(i$DOC.mg.L.1), funs(mean(i$DOC.mg.L.1, na.rm=TRUE)))
}
response = names(split_fullmerge_test$`ARBUTUS LAKE`)[5]
expl = names(fullmerge)[10:30]
response = set_names(response)
response
expl = set_names(expl)
expl

gg <- ggplot(split_fullmerge_test$`ARBUTUS LAKE`, aes(x = Year, y = DOC.mg.L.1, color = split_fullmerge_test$`ARBUTUS LAKE`$Year < 2018)) +
  geom_col() +
  theme(plot.title = element_text(face = "bold", size = 22), axis.text=element_text(size=20),axis.text.x = element_text(angle = 60, hjust = 1, size = 11, face = "bold"), axis.title = element_text(size = 20)) +
  ggtitle("ARBUTUS LAKE") +
  xlab("Year") +
  ylab("DOC in mg/L") +
  scale_x_continuous("Year", labels = as.character(split_fullmerge_test$`ARBUTUS LAKE`$Year), breaks = split_fullmerge_test$`ARBUTUS LAKE`$Year) + 
  theme(axis.text.x = element_text(angle = 60))

gg
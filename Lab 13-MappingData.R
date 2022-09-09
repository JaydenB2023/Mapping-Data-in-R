## Lab 13: Mapping Data in R
## J. Brown
## 2022-04-29

#load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(maps)
library(mapdata)

#Create a dataframe from the "state" boundry file
Bndry <- map_data("state")

#Create an object with a basic map
map1 <- ggplot(data = Bndry) + 
  geom_polygon(aes(x = long, y = lat, group = group),
               color = "black", 
               fill = "purple") + 
  coord_fixed(1.3)
map1

#Create a theme to clean up the map
ditch_the_axes <- theme(axis.text = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        panel.border = element_blank(),
                        panel.grid = element_blank(),
                        axis.title = element_blank(),
                        panel.background = element_blank())

#Print the cleaned up map
map1 + ditch_the_axes + ggtitle("This is the Title of My Map")

##Read in the .csv file with the data
infmort <- read_csv("~/Downloads/InfantMortality2017.csv")

#Classify the infant mortality into a set of ranges for the colors
infmort$classify <- NA
infmort$classify[infmort$inf.mort > 0.0 & infmort$inf.mort <= 4.6] <- "0-4.6"
infmort$classify[infmort$inf.mort > 4.6 & infmort$inf.mort <= 5.8] <- "4.6-5.8"
infmort$classify[infmort$inf.mort > 5.8 & infmort$inf.mort <= 6.2] <- "5.8-6.2"
infmort$classify[infmort$inf.mort > 6.2 & infmort$inf.mort <= 7.1] <- "6.2-7.1"
infmort$classify[infmort$inf.mort > 7.1 & infmort$inf.mort <= 8.7] <- "7.1-8.7"

infantmap <- inner_join(Bndry, infmort, by = "region")

#Create map of infant mortality
map2 <- ggplot(data = infantmap) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = classify),
               color = "white")

#Add the maps together, clean up the axes, and color code the states.
map2 + ditch_the_axes + ggtitle("Infant Mortality in the U.S., 2017") +
  scale_fill_manual(values = c("#26456D", "#2D7CB4", "#CACACA", "#D8392C", "#9B0724"),
                    guide_legend(title = " "))
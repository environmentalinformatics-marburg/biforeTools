cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot Orthoptera observations at Mt. Kilimanjaro
##  
##  Version: 2015-02-14
##  
################################################################################
##
##  Copyright (C) 2015 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "ggmap", "RJSONIO")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Code/bifore/src/csv/"
path.fig <- "Code/bifore/src/figures/"

file.in       <- paste0(path.csv, "lvl0100_biodiversity_data.csv")
file.out.all  <- paste0(path.fig, "lvl0100_map_orthoptera_observations.png")
file.out.year <- paste0(path.fig, "lvl0100_map_orthoptera_observations_year.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}


### Import data ################################################################

data.raw <- read.csv2(file.in,
                     dec = ",",
                     header = TRUE, 
                     stringsAsFactors = TRUE,
                     )

## Read date column as a date
data.raw$date_observation <- as.Date(data.raw$date_observation, format="%Y-%j")

data.sp <- data.raw


### Subsetting #################################################################

data.observations <- data.raw[,1:13]

## Create additional year column
data.observations$year <- as.numeric(format(data.observations$date_observation, "%Y"))


### Plot all orthoptera observations ###########################################

## Define output image | open image port
png(file.out.all, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  size = 1,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "no. of species",
                                     limits=c(0, 30))

style.plot1 <- theme(legend.background = element_rect(colour = "black"),
               plot.title = element_text(size = 20))

labeling.plot1 <- labs(title = "Orthoptera observations 2002 - 2012")

kilimap + orthoptera.obs1 + colourscale + labeling.plot1 + style.plot1 + theme_bw()


## Close image port
graphics.off()


### Plot Orthoptera Observations per year ######################################

## Define output image | open image port
png(file.out.year, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)


kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)

orthoptera.obs1 <- geom_point(aes(x = lon,
                                  y = lat,
                                  colour = nr.of.species),
                              show_guide = FALSE,
                              data = data.observations)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "no. of species",
                                     limits=c(0, 30))

style.plot2 <- theme(legend.background = element_rect(colour = "black"),
                     plot.title = element_text(size = 20))

labeling.plot2 <- labs(title = "Orthoptera observations 2002 - 2012")

kilimap + orthoptera.obs1 + colourscale + facet_wrap(~ year) + style.plot2 + labeling.plot2


# ## Close image port
graphics.off()
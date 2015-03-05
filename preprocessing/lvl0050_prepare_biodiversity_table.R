cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Prepare biodiversity table 
##  
##  - Replace 0-values with NA
##  - Remove observations before MODIS launch date
##  - Remove observations without coordinates
##  - Remove species with less than 10 observations in different plots
##  - Calculate number of species
##  - Add LatLong Coordinates
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
lib <- c("ggplot2", "raster", "rgdal")
lapply(lib, function(...) require(..., character.only = TRUE))


## Set working directory
setwd("/home/sschlauss/")

## Set filepaths
path.csv  <- "Code/bifore/src/csv/"

file.in   <- paste0(path.csv, "abundance_matrix_hemp.csv")
file.out  <- paste0(path.csv, "lvl0050_biodiversity_data.csv")


### Read data ##################################################################

data <- read.csv2(file.in, 
                  header = TRUE, 
                  sep = ";",
                  dec = ".",
                  fill = FALSE, 
                  stringsAsFactors = FALSE)


data$date <- as.Date(data$date, format = "%d.%m.%Y")


### Replace 0-values with NA ###################################################

## Subset data
data.species <- data[, 9:ncol(data)]

## Set 0-values to NA
data.species[data.species == 0] <- NA

## Recombine data
data[, 9:ncol(data)] <- data.species


### Remove observations before MODIS satellite launch ##########################

## Note: MODIS TERRA launch: 1999-12-18
## MODIS AQUA launch: 2002-05-04

modis.date <- as.Date("2002-07-03") # first available MYD02 data
data <- subset(data, date > modis.date)


### Remove observations without coordinates ####################################

data <- data[!is.na(data$coordN | data$coordW), ]


### Calculate number of species ################################################

data$nr.of.species <- apply(data,
                            1,
                            function(x) sum(!is.na(x[9:ncol(data)])))

data <- cbind(data[1:8],
              data$nr.of.species,
              data[10:ncol(data)-1])

colnames(data)[9] <- "nr.of.species"

### Plot single species ########################################################

# plot(data[,9],data$date, type="p")
# 
# qplot(x=data[,9], 
#       y=date, 
#       data=data,
#       geom="jitter",
#       main=colnames(data[9]),
#       xlab="Prevalence",
#       ylab="Time")


### Transform UTM to LatLong coordinates #######################################

data.sp <- data

coordinates(data.sp) <- c("coordW", "coordN")
projection(data.sp) <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
data.sp <- spTransform(data.sp, CRS("+proj=longlat"))

data.sp <- as.data.frame(data.sp)

names(data.sp)[1] <- "lon"
names(data.sp)[2] <- "lat"

## Recombine dataframes
data.all.sp <- cbind(data[1:3],
                     data[6:9],
                     data.sp$lon,
                     data.sp$lat,
                     data$coordW,
                     data$coordN,
                     data[10:ncol(data)])

names(data.all.sp)[8] <- "lon"
names(data.all.sp)[9] <- "lat"
names(data.all.sp)[10] <- "coordW"
names(data.all.sp)[11] <- "coordN"

data <- data.all.sp


### Remove species with less than 1 observation ################################

data.list <- split(data, data$plot)
tmp.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  matrix <- as.matrix(data.list[[i]][, 12:ncol(data.list[[i]])])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

index.species0 <- which(apply(tmp.list, 2, sum, na.rm = TRUE) >= 1) + 11
data0 <- data[, c(1:11, index.species0)]


### Write new csv ##############################################################

write.table(data0, file = file.out, 
            dec = ",", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep = ";")

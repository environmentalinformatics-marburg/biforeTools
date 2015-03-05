cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot gap between observation and cloud-free date, based on lvl0100 dataset.
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
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv      <- "Code/bifore/src/csv/"
path.fig      <- "Code/bifore/src/figures/"

file.in   <- paste0(path.csv, "lvl0100_biodiversity_data.csv")
file.out  <- paste0(path.fig, "lvl0100_diff_days_no-cloud.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}

### Import dataset #############################################################

data.raw <- read.csv2(file.in,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subsetting #################################################################

data <- as.data.frame(data.raw[,4])
names(data) <- "diff_days_no_cloud"


### Plotting ###################################################################

## Define output image | open image port
png(file.out, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

hist <- ggplot(data, aes(x = diff_days_no_cloud)) +
  geom_histogram(binwidth = .5) +
  xlab("days") +
#   ylab("count") +
#   ggtitle("Gap between observations and cloud-free days") +
  theme(plot.title = element_text(lineheight=.8, size = 20)) +
  theme_bw()

hist


## Close image port
graphics.off()
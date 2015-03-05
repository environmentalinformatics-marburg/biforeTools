cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot lvl0400 prevalence data
##  
##  Version: 2015-01-25
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
path.testing  <- paste0(path.csv, "lvl0400_2015-01-24/")


file.in.prevalence  <- paste0(path.testing, "lvl0400_prevalence.csv")
file.out            <- paste0(path.fig, "lvl0400_prevalence.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}


### Import data ################################################################

data.raw <- read.csv2(file.in.prevalence,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Prepare data ###############################################################

## Subset data
data <- data.raw[,-c(1,2)]

## Generating short names; for genera the first two letters to distingusih 
## genera which start with thesame letter
spec.names <- names(data)

names.split <- strsplit(as.character(names(data)), split = "[.]")

for (i in (1:length(data))) {
  spec.names[i] <- paste(substr(names.split[[i]][1], 1, 2),
                         ". ", names.split[[i]][2], sep = "")
}

spec.names <- as.data.frame(spec.names)

## Calculate prevalence.sums
prevalence.sums <- as.data.frame(colSums(data))
names(prevalence.sums) <- "prevalence.sums"

## Combine dataframes
data <- cbind(spec.names,
              prevalence.sums)

## Reorder dataframe
data <- data[with(data, order(-prevalence.sums, spec.names)), ]
rownames(data) <- NULL


### Plot #######################################################################
## Define output image | open image port
png(file.out, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot <- ggplot(data, aes(x=reorder(spec.names, prevalence.sums), y=prevalence.sums)) + 
  geom_bar(stat="identity") +
  scale_fill_grey() +
  xlab("") +
  ylab("Sums of prevalence") +
  ylim(0,130) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(face = 'italic'))

plot

## Close image port
graphics.off()
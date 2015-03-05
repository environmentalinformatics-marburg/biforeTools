cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot kappa vs. prevalence
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

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv    <- "Code/bifore/src/csv/"
path.fig    <- "Code/bifore/src/figures/"
path.testing  <- paste0(path.csv, "lvl0400_2015-01-24/")

file.400.in <- paste0(path.testing, "lvl0400_prevalence.csv")
file.410.in <- paste0(path.testing, "lvl0410_kappa.csv")
file.out    <- paste0(path.fig, "lvl0410_kappa-prevalence.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}

### Import data ################################################################

data.400.raw <- read.csv2(file.400.in, header = TRUE)
data.410.raw <- read.csv2(file.410.in, header = TRUE)


### Prepare plot ###############################################################

## Subset data
data.400 <- data.400.raw[,-c(1,2)]
data.410 <- data.410.raw[,-c(1,2)]


## Calculate prevalence sums
data.400.prevalence <- data.frame(colSums(data.400))
names(data.400.prevalence) <- "prevalence.sum"
## Calculate mean kappa
data.410.kappa <- data.frame(colMeans(data.410))
names(data.410.kappa) <- "mean.kappa"

## Combine dataframes
data <- cbind(data.400.prevalence, data.410.kappa)

model <- lm(data$mean.kappa~data$prevalence.sum)
summary(model)
### Plot #######################################################################

## Define output image | open image port
png(file.out, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

plot <- ggplot(data, aes(x=prevalence.sum, y=mean.kappa)) + 
  geom_point() +
  stat_smooth(method="lm", formula=y~x, n=34, level = .95) +
#   geom_abline(intercept = model$coefficients[1], 
#               slope = model$coefficients[2], 
#               colour = "red",
#               size = 1.3) +
  geom_text(data = data, x = 104, y = 0.22, label = "p-value = 0.48") +
  xlab("Sums of prevalence") +
  ylab("Mean Kappa") +
  theme_bw()

plot


## Close image port
graphics.off()
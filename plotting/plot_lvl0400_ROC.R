cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  ROC curve for single species lvl0400 predictions
##  
##  Version: 2015-01-24
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
lib <- c("ROCR")

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv 		<- "Code/bifore/src/csv/"
path.fig 		<- "Code/bifore/src/figures/"
path.testing 	<- paste0(path.csv, "lvl0400_2015-01-24/")

file.in.prediction <- paste0(path.testing, "lvl0400_prediction_25test.csv")
file.out 		   <- paste0(path.fig, "lvl0400_ROC_tpr-fpr.png")

if (!file.exists(path.fig)) {dir.create(file.path(path.fig))}

### Import data ################################################################

data.raw <- read.csv2(file.in.prediction,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset & reconstruct prediction data #######################################

## Subset prediction probabilities
df.predict.prob <- data.raw[grep("prob", data.raw$parameters), ]  # prediction probabilities (test data)

## Subset prediction classes
df.observed.class <- data.raw[grep("observed", data.raw$parameters), ]  # observed classes (test data)

## Reconstruct prediciton class factor levels
for(i in 3:ncol(df.observed.class)) {
  df.observed.class[, i] <- as.factor(ifelse(df.observed.class[, i] >= 1,
                                             "yes","no"))
  
  df.observed.class[, i] <- factor(df.observed.class[, i], 
                                   levels = c("yes", "no"))
  
}


### Select species for ROC Curve ###############################################

names(df.predict.prob)
names(df.predict.prob[29])  # replace no. of column


### ROCR prediction ############################################################

pred <- prediction(predictions = df.predict.prob[, 29],  # replace no. of column
                   labels = df.observed.class[, 29])     # replace no. of column


### ROCR performance ###########################################################

perf.tpr.fnr <- performance(pred,"tpr","fpr")  # TruePositiveRate ~ FalsePositiveRate

## Extract Area under curve value
perf_auc <- performance(pred, "auc")  # Area under curve
perf_auc@y.values[[1]]  # print AUC value


### Plot #######################################################################

## Define output image | open image port
# png(file.out, 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot(perf.tpr.fnr)
lines(c(0,1),c(0,1), col = "grey")
legend("bottomright", lwd = 1, legend = c(round(perf_auc@y.values[[1]], 2)), bty = "n")

## Close image port
# graphics.off()

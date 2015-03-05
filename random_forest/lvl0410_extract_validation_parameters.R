cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Extraction of several valiadation parameters of lvl0400 data
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
lib <- c()

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Code/bifore/src/csv/"
path.testing <- paste0(path.csv, "lvl0400_2015-01-24/")

file.in.validation  <- paste0(path.testing,"lvl0400_validation_25test.csv")
file.out.accuracy   <- paste0(path.testing, "lvl0410_accuracy.csv")
file.out.kappa      <- paste0(path.testing, "lvl0410_kappa.csv")



### Import data ################################################################

data.raw <- read.csv2(file.in.validation,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset data ################################################################

## Subset accuracy
df.accuracy <- data.raw[grep("Accuracy", data.raw$parameters), ]  

## Subset kappa
df.kappa <- data.raw[grep("Kappa", data.raw$parameters), ]


### Write csv files ############################################################

write.table(df.accuracy,
            file = file.out.accuracy,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.kappa,
            file = file.out.kappa,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")
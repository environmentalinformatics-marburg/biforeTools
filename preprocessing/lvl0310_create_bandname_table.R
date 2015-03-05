cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Creates a table with MODIS bandnames.
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

## Set working directory
setwd("/home/sschlauss/")

## Set filepath
path.csv  <- "Code/bifore/src/csv/"

file.out  <- paste0(path.csv, "bandnames.csv")


### Create bandname sequences ##################################################

## "raw bandno."

number <- c()
for (i in seq(1:9)) number[i] <- paste0("0", i)
for (i in seq(10:12)) number[i + 9] <- i + 9
number[13:16] <- c("13.1",
                   "13.2",
                   "14.1",
                   "14.2")
for (i in seq(15:36)) number[i + 16] <- i + 14


## "band_"
bands <- c()
for (i in seq(1:9)) bands[i] <- paste0("band_0", i)
for (i in seq(10:12)) bands[i + 9] <- paste0("band_", i + 9)
bands[13:16] <- c("band_13.1",
                  "band_13.2",
                  "band_14.1",
                  "band_14.2")
for(i in seq(15:36)) bands[i + 16] <- paste0("band_", i + 14)


## "greyvalue_band_"
greyval_bands <- c()
for (i in seq(1:9)) greyval_bands[i] <- paste0("greyvalue_band_0", i)
for (i in seq(10:12)) greyval_bands[i + 9] <- paste0("greyvalue_band_", i + 9)
greyval_bands[13:16] <- c("greyvalue_band_13.1",
                          "greyvalue_band_13.2",
                          "greyvalue_band_14.1",
                          "greyvalue_band_14.2")
for(i in seq(15:36)) greyval_bands[i + 16] <- paste0("greyvalue_band_", i + 14)


### Combine bandname sequences in a dataframe ##################################

df.bandnames <- cbind.data.frame(number, 
                                 bands, 
                                 greyval_bands)


# df.bandnames <- cbind.data.frame(number, 
#                                  bands, 
#                                  greyval_bands, 
#                                  diff_bands, 
#                                  sd_bands)


### Write .csv #################################################################

write.table(df.bandnames,
            file = file.out,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

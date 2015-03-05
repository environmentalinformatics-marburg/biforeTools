cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Compare downloaded MYD02 with nocloud dates (.csv)
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

lib <- c("foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/sschlauss/")

## Detect available CPU cores
ncores <- detectCores()


### Set filepaths ##############################################################

path.csv  <- "Code/bifore/src/csv/"
path.sat  <- "Code/bifore/src/sat/"

path.biodiversity.csv <- paste0(path.csv, "lvl0100_biodiversity_data.csv")
path.myd02            <- paste0(path.sat, "myd02/")
path.myd03            <- paste0(path.sat, "myd03-35_hdf_daytime/")
path.myd02_03         <- paste0(path.sat, "myd02-03_hdf/")

## Create folders
if (!file.exists(path.myd02_03)) {dir.create(file.path(path.myd02_03))}


### Copy MYD02 #################################################################

list.rawpath.myd02 <- list.files(path.myd02, 
                                 pattern = "MYD02", 
                                 full.names = TRUE)

## Copy MYD02 files
if (length(list.rawpath.myd02)  > 0) {
  registerDoParallel(cl <- makeCluster(ncores))
  foreach (m = list.rawpath.myd02, .packages = lib) %dopar% {
    file.rename (from = m,
                 to = paste0(path.myd02_03, basename(m)))
  }
  stopCluster(cl)
}


### Import biodiversity dataset ################################################

data <- read.csv2(path.biodiversity.csv,
                  dec = ",",
                  header = TRUE, 
                  stringsAsFactors = FALSE)


### Extract MYD02 date string ##################################################

## List hdf files
lst.1km <- list.files(path.myd02_03,
                      pattern = "MYD021KM",
                      full.names = TRUE)
  
lst.qkm <- list.files(path.myd02_03,
                      pattern = "MYD02QKM",
                      full.names = TRUE)

lst.hkm <- list.files(path.myd02_03,
                      pattern = "MYD02HKM",
                      full.names = TRUE)


## Extract date string from filename
dates.1km <- substr(basename(lst.1km), 11, 22)
dates.1km <- paste0(substr(dates.1km, 0, 4),
                    "-",
                    substr(dates.1km, 5, 7),
                    "_",
                    substr(dates.1km, 9, 13))

dates.qkm <- substr(basename(lst.qkm), 11, 22)
dates.qkm <- paste0(substr(dates.qkm, 0, 4),
                    "-",
                    substr(dates.qkm, 5, 7),
                    "_",
                    substr(dates.qkm, 9, 13))

dates.hkm <- substr(basename(lst.hkm), 11, 22)
dates.hkm <- paste0(substr(dates.hkm, 0, 4),
                    "-",
                    substr(dates.hkm, 5, 7),
                    "_",
                    substr(dates.hkm, 9, 13))


dates.1km
dates.qkm
dates.hkm


### Check if downloaded MYD02 files are complete ###############################

data$date_nocloud %in% dates.1km
data$date_nocloud %in% dates.qkm
data$date_nocloud %in% dates.hkm

dates.1km %in% data$date_nocloud
dates.qkm %in% data$date_nocloud
dates.hkm %in% data$date_nocloud


### Copy MYD03 files ###########################################################

lst.myd03 <- list.files(path.myd03, 
                        pattern = "MYD03", 
                        full.names = TRUE)

for (i in lst.myd03)
  for (h in lst.1km)
    if (substr(basename(i), 8, 19) == substr(basename(h), 11, 22))
      file.rename(from = i,
                  to = paste0(path.myd02_03, basename(i)))


### Copy MYD02 files ###########################################################

for (g in lst.1km)
  file.rename(from = g,
              to = paste0(path.myd02_03, basename(g)))

for (f in lst.hkm)
  file.rename(from = f,
              to = paste0(path.myd02_03, basename(f)))

for (e in lst.qkm)
  file.rename(from = e,
              to = paste0(path.myd02_03, basename(e)))


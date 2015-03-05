cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Check coordinates for clouds using MODIS CLOUDMASK and MODISCLOUD-package
## 
##  Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf
##        - MYD35 .hdf metadata
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
lib <- c("modiscloud", 
         "devtools", 
         "rgdal", 
         "raster", 
         "foreach", 
         "doParallel", 
         "ggplot2")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/sschlauss/")

## Detect available CPU cores
ncores <- detectCores()


### Set filepaths ##############################################################

## To preprocess MYD35_L2 and MYD03 files; both must be in the same directory.

path.csv  <- "Code/bifore/src/csv/"
path.sat  <- "Code/bifore/src/sat/"
path.mrt  <- "apps/MRTSwath/bin/swath2grid"

path.biodiversity.csv <- paste0(path.csv, "lvl0050_biodiversity_data.csv")
path.nocloud.csv      <- paste0(path.csv, "lvl0100_biodiversity_data.csv")
path.hdf.in           <- paste0(path.sat, "myd03-35_hdf/")
path.hdf.sub          <- paste0(path.sat, "myd03-35_hdf_daytime/")
path.hdf.myd03        <- paste0(path.sat, "myd03/")
path.hdf.myd35        <- paste0(path.sat, "myd35/")
path.tif.cloudmask    <- paste0(path.sat, "myd_cloudmask_tif_daytime/")


## Load required modules
source("Code/bifore/preprocessing/modules/lvl0110_writeMRTSwathParamFile_cloudcheck.R")
source("Code/bifore/preprocessing/modules/lvl0110_runSwath2Grid.R")

## Create folders
if (!file.exists(path.hdf.in)) {dir.create(file.path(path.hdf.in))}
if (!file.exists(path.hdf.sub)) {dir.create(file.path(path.hdf.sub))}
if (!file.exists(path.tif.cloudmask)) {dir.create(file.path(path.tif.cloudmask))}


### Import biodiversity dataset ################################################

data <- read.csv2(path.biodiversity.csv,
                  dec = ",",
                  header = TRUE, 
                  stringsAsFactors = TRUE)

## Read date column as a date
data$date <- strftime(as.POSIXct(data$date, 
                                 format = "%Y-%m-%d"), 
                      format = "%Y%j")

data.orig <- data

## Runtime calculation
starttime <- Sys.time()


### Copy MYD03 & MYD35 HDF files into the same folder ##########################

list.rawpath.myd03 <- list.files(path.hdf.myd03, 
                                 pattern = "MYD03", 
                                 full.names = TRUE)

list.rawpath.myd35 <- list.files(path.hdf.myd35, 
                                 pattern = "MYD35", 
                                 full.names = TRUE)

## Copy MYD03 files
if (length(list.rawpath.myd03)  > 0) {
  registerDoParallel(cl <- makeCluster(ncores))
  foreach (m = list.rawpath.myd03, .packages = lib) %dopar% {
    file.rename (from = m,
                 to = paste0(path.hdf.in, basename(m)))
  }
  stopCluster(cl)
}

## Copy MYD35 files
if (length(list.rawpath.myd35)  > 0) {
  registerDoParallel(cl <- makeCluster(ncores))
  foreach (n = list.rawpath.myd35, .packages = lib) %dopar% {
    file.rename (from = n,
                 to = paste0(path.hdf.in, basename(n)))
  }
  stopCluster(cl)
}


### Separate day and night hdf files ###########################################

fls.myd03 <- list.files(path.hdf.in,
                        pattern = "MYD03",
                        full.names = TRUE)

fls.myd35 <- list.files(path.hdf.in,
                        pattern = "MYD35",
                        full.names = TRUE)

registerDoParallel(cl <- makeCluster(ncores))

## Move MYD03 *.hdf files
foreach(a = fls.myd03, .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(a), 16, 19)) < 1700)
    file.rename(from = a,
                to = paste0(path.hdf.sub, basename(a)))
}


## Move MYD35 *.hdf files
foreach(b = fls.myd35, .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(b), 19, 22)) < 1700)
    file.rename(from = b,
                to = paste0(path.hdf.sub, basename(b)))
}

stopCluster(cl)


### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########

## List MYD-files
list.files(path = path.hdf.sub, pattern = "MYD")

## Get the matching data/geolocation file pairs
fls.matching <- check_for_matching_geolocation_files(moddir = path.hdf.sub,
                                                     modtxt = "MYD35_L2",
                                                     geoloctxt = "MYD03",
                                                     return_geoloc = FALSE,
                                                     return_product = FALSE)
fls.matching


## Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


### Convert .hdf to .tif #######################################################

for(i in 1:nrow(fls.matching)) {
  
  ## Write parameter file for each .hdf
  prmfn = writeMRTSwathParamFile_cloud(prmfn = "tmpMRTparams.prm", 
                                       tifsdir = path.tif.cloudmask, 
                                       modfn = fls.matching$mod35_L2_fns[i], 
                                       geoloc_fn = fls.matching$mod03_fns[i], 
                                       ul_lon = ul_lon, 
                                       ul_lat = ul_lat, 
                                       lr_lon = lr_lon, 
                                       lr_lat = lr_lat)
  
  runSwath2Grid(mrtpath = path.mrt, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = path.tif.cloudmask, 
                modfn = ffls.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


### Check observations from *.csv file for cloudiness using tiffs ##############

## Get .tif list from swath2grid output
tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)

# tiffns
                   
## Convert data to spatial object
coordinates(data) <- ~ lon + lat
# coordinates(data) <- c("lon","lat")
proj4string(data) <- CRS("+init=epsg:4326")
 
## Parallelization
registerDoParallel(cl <- makeCluster(ncores))

## Loop through all available dates
myd02.lst <- foreach(g = 1:nrow(data), 
                     .packages = lib, 
                     .combine = function(...) as.data.frame(rbind(...), 
                                                            stringsAsFactors = FALSE)) %dopar% {
  
  ## Initialize while-loop
  current.date <- data$date[g]
  cloudy <- TRUE
  
  
  while (cloudy) {
    
    ## List available *_b0.tif files of current date
    fls.avl <- tiffns[grep(current.date, substr(basename(tiffns), 1, 21))]
    fls.avl.b0 <- fls.avl[grep("b0", fls.avl)]
    
    ## Import raster images
    rst.avl.b0 <- lapply(fls.avl.b0, raster)
    
    for(h in rst.avl.b0) {
      
      ## Extract corresponding cell values from raster images
      val.avl.b0 <- extract(h, data[g, ])
      
      ## Convert extracted values to binary format
      bit.avl.b0 <- rev(t(digitsBase(val.avl.b0, base = 2, 8)))
      
      ## Cloud Indicator from myd35 metadata
      ## Unobstructed FOV Quality Flag
      ## 00 = Cloudy
      ## 01 = Uncertain
      ## 10 = Probably Clear
      ## 11 = Confident Clear
      
      ## Break out of for-loop in case of cloud absence
      cloudindicator <- as.numeric(paste0(bit.avl.b0[2],bit.avl.b0[3]))
      
      if (cloudindicator == 11) {
        cloudy <- FALSE
        break
      }
    }
    
    ## Increment date by 1 day in case of no cloud absence
    if (cloudy) {
      
      current.date <- as.Date(current.date, format = "%Y%j") + 1
      current.date <- strftime(current.date, format = "%Y%j")
    }
  }
  
  ## Retrieve information about first cloud-free day
  return(substr(names(h), 11, 22))
}


## Deregister parallel backend
stopCluster(cl)


### Create new .csv with no-cloud date and diff-days ###########################

len <- length(names(data.orig))
len

data.clouddates <- data.orig

myd02.lst
names(myd02.lst) <- "date_nocloud"
names(data.clouddates)[2] <- "date_observation"

## Create no-cloud date column
data.clouddates$date_nocloud <- myd02.lst$date_nocloud

## Create diff-days column
data.clouddates$diff_days_nocloud <- as.numeric(as.Date(data.clouddates$date_nocloud, format = "%Y%j.%H%M") - as.Date(data.clouddates$date_observation, format = "%Y%j"))

## Reorder df
data.clouddates <- cbind(data.clouddates[1:2],
                         data.clouddates[ncol(data.clouddates)-1],
                         data.clouddates[ncol(data.clouddates)],
                         data.clouddates[3:11],
                         data.clouddates[12:len])
names(data.clouddates)

## Reformat dates
for(i in 1:nrow(data.clouddates)) {
  data.clouddates$date_nocloud[i] <- paste0(substr(data.clouddates$date_nocloud[i], 1, 4), 
                                      "-", 
                                      substr(data.clouddates$date_nocloud[i], 5, 7), 
                                      "_", 
                                      substr(data.clouddates$date_nocloud[i], 9, 13))
}

for(i in 1:nrow(data.clouddates)) {
  data.clouddates$date_observation[i] <- paste0(substr(data.clouddates$date_observation[i], 1, 4), 
                                          "-", 
                                          substr(data.clouddates$date_observation[i], 5, 7))
}


## Write new *.csv
write.table(data.clouddates, 
            file = path.nocloud.csv,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

## Plot diff_days_no-cloud
qplot(x = diff_days_nocloud,
      data = data.clouddates,
      geom = "histogram",
      binwidth = 0.5)

### Runtime calculation ########################################################
endtime <- Sys.time()
time <- endtime - starttime
time

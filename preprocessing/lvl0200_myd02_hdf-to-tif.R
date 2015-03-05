cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Convert MYD02 .hdf files to .geotiff
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
lib <- c("modiscloud", "devtools", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/sschlauss/")


### Set filepaths ##############################################################

path.hdf.in   <- "Code/bifore/src/sat/myd02-03_hdf/"
path.tif      <- "Code/bifore/src/sat/myd02_tif/"
path.modules  <- "Code/bifore/preprocessing/modules/"
path.mrt      <- "apps/MRTSwath/bin/swath2grid"

## Create folders
if (!file.exists(path.hdf.in)) {dir.create(file.path(path.hdf.in))}
if (!file.exists(path.tif)) {dir.create(file.path(path.tif))}

## Load required modules
source(paste0(path.modules, "lvl0210_writeMRTSwathParamFile_1000.R"))
source(paste0(path.modules, "lvl0210_writeMRTSwathParamFile_500.R"))
source(paste0(path.modules, "lvl0210_writeMRTSwathParamFile_250.R"))
source(paste0(path.modules, "lvl0220_runSwath2Grid.R"))
source(paste0(path.modules, "lvl0230_renameSuffix.R"))

## Runtime calculation
starttime <- Sys.time()


### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########

### List hdf files
fls.myd <- list.files(path.hdf.in,
                      pattern = "MYD",
                      full.names = TRUE)

fls.myd

# Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


### For-loop .hdf to .tif 1000m per pixel ######################################

## Get the matching data/geolocation file pairs
fls.1km.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD021KM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.1km.matching


## Write parameterfile for MRTswath tool
for (i in 1:nrow(fls.1km.matching)) {
  prmfn = writeMRTSwathParamFile_1000(prmfn = "tmpMRTparams.prm", 
                                      tifsdir = path.tif, 
                                      modfn = fls.1km.matching$mod35_L2_fns[i], 
                                      geoloc_fn = fls.1km.matching$mod03_fns[i], 
                                      ul_lon = ul_lon, 
                                      ul_lat = ul_lat, 
                                      lr_lon = lr_lon, 
                                      lr_lat = lr_lat)
  
  ## Convert .hdf to .geotiff using MRTswath tool
  runSwath2Grid(mrtpath = path.mrt, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = path.tif, 
                modfn = fls.1km.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.1km.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


### For-loop .hdf to .tif 500m per pixel #######################################

## Get the matching data/geolocation file pairs
fls.hkm.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD02HKM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.hkm.matching

## Write parameterfile for MRTswath tool
for (i in 1:nrow(fls.hkm.matching)) {
  prmfn = writeMRTSwathParamFile_500(prmfn = "tmpMRTparams.prm", 
                                     tifsdir = path.tif, 
                                     modfn = fls.hkm.matching$mod35_L2_fns[i], 
                                     geoloc_fn = fls.hkm.matching$mod03_fns[i], 
                                     ul_lon = ul_lon, 
                                     ul_lat = ul_lat, 
                                     lr_lon = lr_lon, 
                                     lr_lat = lr_lat)
  
  ## Convert .hdf to .geotiff using MRTswath tool
  runSwath2Grid(mrtpath = path.mrt, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = path.tif, 
                modfn = fls.hkm.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.hkm.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


### For-loop .hdf to .tif 250m per pixel #######################################

# Get the matching data/geolocation file pairs
fls.qkm.matching = check_for_matching_geolocation_files(moddir = path.hdf.in,
                                                        modtxt = "MYD02QKM",
                                                        geoloctxt = "MYD03", 
                                                        return_geoloc = FALSE, 
                                                        return_product = FALSE)
fls.qkm.matching

## Write parameterfile for MRTswath tool
for (i in 1:nrow(fls.qkm.matching)) {
  prmfn = writeMRTSwathParamFile_250(prmfn = "tmpMRTparams.prm", 
                                     tifsdir = path.tif, 
                                     modfn = fls.qkm.matching$mod35_L2_fns[i], 
                                     geoloc_fn = fls.qkm.matching$mod03_fns[i], 
                                     ul_lon = ul_lon, 
                                     ul_lat = ul_lat, 
                                     lr_lon = lr_lon, 
                                     lr_lat = lr_lat)
  
  ## Convert .hdf to .geotiff using MRTswath tool
  runSwath2Grid(mrtpath = path.mrt, 
                prmfn = "tmpMRTparams.prm", 
                tifsdir = path.tif, 
                modfn = fls.qkm.matching$mod35_L2_fns[i], 
                geoloc_fn = fls.qkm.matching$mod03_fns[i], 
                ul_lon = ul_lon, 
                ul_lat = ul_lat, 
                lr_lon = lr_lon, 
                lr_lat = lr_lat)
}


### Rename .tif to proper filename e.g.: *_B20.tif #############################

lst.tif <- list.files(path.tif, pattern=".tif", full.names=TRUE)

suffixes.in <- c("1KM_Emissive_b0.tif", 
                 "1KM_Emissive_b1.tif",
                 "1KM_Emissive_b2.tif",
                 "1KM_Emissive_b3.tif",
                 "1KM_Emissive_b4.tif",
                 "1KM_Emissive_b5.tif",
                 "1KM_Emissive_b6.tif",
                 "1KM_Emissive_b7.tif",
                 "1KM_Emissive_b8.tif",
                 "1KM_Emissive_b9.tif",
                 "1KM_Emissive_b10.tif",
                 "1KM_Emissive_b11.tif",
                 "1KM_Emissive_b12.tif",
                 "1KM_Emissive_b13.tif",
                 "1KM_Emissive_b14.tif",
                 "1KM_Emissive_b15.tif",
                 "1KM_RefSB_b0.tif",
                 "1KM_RefSB_b1.tif",
                 "1KM_RefSB_b2.tif",
                 "1KM_RefSB_b3.tif",
                 "1KM_RefSB_b4.tif",
                 "1KM_RefSB_b5.tif",
                 "1KM_RefSB_b6.tif",
                 "1KM_RefSB_b7.tif",
                 "1KM_RefSB_b8.tif",
                 "1KM_RefSB_b9.tif",
                 "1KM_RefSB_b10.tif",
                 "1KM_RefSB_b11.tif",
                 "1KM_RefSB_b12.tif",
                 "1KM_RefSB_b13.tif",
                 "1KM_RefSB_b14.tif",
                 "250_RefSB_b0.tif",
                 "250_RefSB_b1.tif",
                 "500_RefSB_b0.tif",
                 "500_RefSB_b1.tif",
                 "500_RefSB_b2.tif",
                 "500_RefSB_b3.tif",
                 "500_RefSB_b4.tif")                   

suffixes.out <- c("B20", 
                  "B21",
                  "B22",
                  "B23",
                  "B24",
                  "B25",
                  "B27",
                  "B28",
                  "B29",
                  "B30",
                  "B31",
                  "B32",
                  "B33",
                  "B34",
                  "B35",
                  "B36",
                  "B08",
                  "B09",
                  "B10",
                  "B11",
                  "B12",
                  "B13.1",
                  "B13.2",
                  "B14.1",
                  "B14.2",
                  "B15",
                  "B16",
                  "B17",
                  "B18",
                  "B19",
                  "B26",
                  "B01",
                  "B02",
                  "B03",
                  "B04",
                  "B05",
                  "B06",
                  "B07")

## Call renameSuffix function
foreach(i = suffixes.in, j = suffixes.out) %do% {
  renameSuffix(files = lst.tif, 
               suffix.in = i, 
               suffix.out = j, 
               path.tif)
}


### Runtime calculation ########################################################

endtime <- Sys.time()
time <- endtime - starttime
time

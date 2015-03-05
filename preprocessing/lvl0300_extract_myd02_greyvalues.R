cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
##  Extract greyvalues from MODIS .geotiff images for each observation (lvl0100).
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
lib <- c("rgdal", "doParallel", "raster", "matrixStats", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/sschlauss/")

ncores <- detectCores()


### Set filepaths ##############################################################

path.csv      <- "Code/bifore/src/csv/"
path.sat      <- "Code/bifore/src/sat/"
path.modules  <- "Code/bifore/preprocessing/modules/"

path.hdf                  <- paste0(path.sat, "myd02-03_hdf/")
path.tif                  <- paste0(path.sat, "myd02_tif/")
path.tif.na               <- paste0(path.sat, "myd02_tif_na/")
path.tif.calc             <- paste0(path.sat, "myd02_tif_calc/")
path.biodiversity.csv     <- paste0(path.csv, "lvl0100_biodiversity_data.csv")
path.biodiversity.csv.out <- paste0(path.csv, "lvl0300_biodiversity_data.csv")

## Source modules
source(paste0(path.modules, "lvl0320_hdfExtractScales.R"))

## Create folders
if (!file.exists(path.tif)) {dir.create(file.path(path.tif))}
if (!file.exists(path.tif.na)) {dir.create(file.path(path.tif.na))}
if (!file.exists(path.tif.calc)) {dir.create(file.path(path.tif.calc))}

### Import biodiversity dataset ################################################

data.bio.raw <- read.csv2(path.biodiversity.csv,
                          dec = ",",
                          header = TRUE, 
                          stringsAsFactors = FALSE)

## Runtime calculation
starttime <- Sys.time()


### Get MYD bandnames ##########################################################

## Set date for extraction
tmp.date <- data.bio.raw$date_nocloud[1]

## Reformat date
tmp.date <- paste0(substr(tmp.date, 1, 4),
                   substr(tmp.date, 6, 8),
                   ".",
                   substr(tmp.date, 10, 13))

lst.hdf.1km <- list.files(path.hdf, 
                          pattern = paste("1KM", tmp.date, sep = ".*"), 
                          full.names = TRUE)
lst.hdf.hkm <- list.files(path.hdf, 
                          pattern = paste("HKM", tmp.date, sep = ".*"), 
                          full.names = TRUE)
lst.hdf.qkm <- list.files(path.hdf, 
                          pattern = paste("QKM", tmp.date, sep = ".*"), 
                          full.names = TRUE)

## Extract bandnames from *.hdf files
bandnames <- hdfExtractMODScale (lst.hdf.qkm,
                                 lst.hdf.hkm,
                                 lst.hdf.1km)
bandnames <- bandnames$bands


### List .hdf and .tif for specific date and import .tif as RasterLayer Object #

## List unique cloudfree dates
lst.date <- unique(data.bio.raw$date_nocloud)
lst.date

## Parallelization
registerDoParallel(cl <- makeCluster(ncores))

foreach(a = lst.date, .packages = lib) %dopar% {
  
  ## Extract date from biodiversity data
  tmp.date <- a
  
  ## Reformat date
  tmp.date <- paste0(substr(tmp.date, 1, 4),
                     substr(tmp.date, 6, 8),
                     ".",
                     substr(tmp.date, 10, 13))
  
  ## List .tif files
  lst.tif <- list.files(path.tif, pattern = tmp.date, full.names = TRUE)
  
  ## Import .tif files as RasterLayer objects
  lst.tif.raster <- lapply(lst.tif, raster)
  
  ### List .hdf files
  lst.hdf.1km <- list.files(path.hdf, 
                            pattern = paste("1KM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  lst.hdf.hkm <- list.files(path.hdf, 
                            pattern = paste("HKM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  lst.hdf.qkm <- list.files(path.hdf, 
                            pattern = paste("QKM", tmp.date, sep = ".*"), 
                            full.names = TRUE)
  
  
  ### Check raster for NA values ###############################################
  
  # REFERING TO: Level 1B Product Data Dictionary V6.1.14
  # http://mcst.gsfc.nasa.gov/content/l1b-documents
  # http://mcst.gsfc.nasa.gov/sites/mcst.gsfc/files/file_attachments/M1055_PDD_D_072712final.pdf
  # (P.80f)
  # ...
  # The valid range is [0-32767], inclusive.  Any value above 32767 represents 
  # unusable data.  Table 2.2.3 shows the meaning of data values over 32767.
  # Table 2.2.3:  Meaning of Data Values Outside of Valid Range
  # 
  # Value  Meaning
  # --------------------------------------------------------------------------------
  # 65535   Fill Value (includes reflective band data at night mode and completely missing L1A scans)
  # 65534	 L1A DN is missing within a scan
  # 65533	 Detector is saturated
  # 65532	 Cannot compute  zero point DN
  # 65531	 Detector is dead (see comments below)
  # 65530	 RSB dn** below the minimum of the scaling range
  # 65529	 TEB radiance or RSB dn** exceeds the maximum of the scaling range
  # 65528	 Aggregation algorithm failure
  # 65527	 Rotation of Earth view Sector from nominal science collection position
  # 65526	 Calibration coefficient b1 could not be computed
  # 65501 - 65525	(reserved for future use)
  # 65500	 NAD closed upper limit
  
  
  print(paste0(tmp.date, " - Check raw raster files for NA values"))
  
  ## Set all values outside valid range NA
  foreach (r = lst.tif.raster, n = lst.tif) %do% {
    values(r)[values(r) > 32767] <- NA
    writeRaster(r, filename = paste0(path.tif.na, basename(n)), overwrite = TRUE)
  }
  
  
  ### Extraction of radiance-, reflectance scale and offset from *.hdf #########
  
  print(paste0(tmp.date, " - Extract scalefactors and offset from *.hdf"))
  
  ## Function call
  modscales <- hdfExtractMODScale (lst.hdf.qkm,
                                   lst.hdf.hkm,
                                   lst.hdf.1km)
  
  
  ## Calculate new greyvalues (greyvalue * scalefactor) and write to new raster
  print(paste0(tmp.date, " - Calculate new greyvalues"))
  
  lst.tif.na <- list.files(path.tif.na, pattern = tmp.date, full.names = TRUE)
  lst.tif.na.raster <- lapply(lst.tif.na, raster)
  
  foreach(r = as.list(lst.tif.na.raster), 
          s = as.list(as.numeric(modscales[["scales"]])),
          t = as.list(lst.tif.na),
          o = as.list(as.numeric(modscales[["offsets"]]))) %do% {
            calc(r, fun = function(x) s * (x - o), 
                 filename = paste0(path.tif.calc, basename(t)), 
                 format = "GTiff", 
                 overwrite = TRUE)        
          }
}  
stopCluster(cl)


### Extraction of cell values ##################################################

## List cloud-free dates from biodiversity dataset
lst.nocloud <- as.list(data.bio.raw$date_nocloud)

## Import biodiversity dataset as SpatialPointsDataframe objects
data.bio.sp <- data.bio.raw
coordinates(data.bio.sp) <- c("lon", "lat")
projection(data.bio.sp) <- "+init=epsg:4326"

## Extract date from biodiversity data
registerDoParallel(cl <- makeCluster(ncores))
greyvalues <- foreach(a = lst.nocloud, 
                      b = seq(nrow(data.bio.sp)), 
                      .combine = "rbind", 
                      .packages = lib) %dopar% {
  
  tmp.date <- a
  
  ## Reformat date
  tmp.date <- paste0(substr(tmp.date, 1, 4),
                     substr(tmp.date, 6, 8),
                     ".",
                     substr(tmp.date, 10, 13)) 
  
  
  ## List calculated raster images
  lst.tif.calc <- list.files(path.tif.calc, pattern = tmp.date, full.names = TRUE)
  lst.tif.calc.raster <- lapply(lst.tif.calc, raster)
  
  ## Extract cell values from all bands
  greyvalues.calc <- foreach(r = seq(lst.tif.calc.raster), 
                             .combine = "cbind") %do% {
                               lst.tif.calc.raster[[r]][cellFromXY(lst.tif.calc.raster[[r]], 
                                                                   data.bio.sp[b, ])]
                             } 
}

stopCluster(cl)

## Create dataframe with extracted greyvalues
greyvalues <- data.frame(greyvalues)

## Set names of greyvalues dataframe
names(greyvalues) <- foreach(i = bandnames, j = names(greyvalues)) %do% {
  j <- paste0("greyval_band_", i)
}

names(greyvalues)


# ### Calculate first derivate (diff) ############################################
# 
# ## Calculate diff
# diff <- as.data.frame(rowDiffs(as.matrix(greyvalues)))
# diff <- cbind(0,diff)
# 
# ## Set names for diffs
# names(diff) <- foreach(i = bandnames, 
#                        j = names(diff)) %do% {
#                          j <- paste0("deriv_band_", i)
#                        }
# 
# names(diff)
# 
# 
# ### Pixelraster ################################################################
# 
# ## List cloud-free dates from biodiversity dataset
# lst.nocloud <- as.list(data.bio.raw$date_nocloud)
# 
# ## Import biodiversity dataset as SpatialPointsDataframe objects
# data.bio.sp <- data.bio.raw
# coordinates(data.bio.sp) <- c("lon", "lat")
# projection(data.bio.sp) <- "+init=epsg:4326"
# 
# registerDoParallel(cl <- makeCluster(ncores))
# matrix.sd <- foreach(a = lst.nocloud, b = seq(nrow(data.bio.sp)), .combine = "rbind", .packages = lib) %dopar% {
#   
#   tmp.date <- a
#   
#   ## Reformat date
#   tmp.date <- paste0(substr(tmp.date, 1, 4),
#                      substr(tmp.date, 6, 8),
#                      ".",
#                      substr(tmp.date, 10, 13)) 
#   
#   
#   ## List calculated tifs
#   lst.tif.calc.1km <- list.files(path.tif.calc, pattern = paste("1KM", tmp.date, sep = ".*"), full.names = TRUE)
#   lst.tif.calc.hkm <- list.files(path.tif.calc, pattern = paste("HKM", tmp.date, sep = ".*"), full.names = TRUE)
#   lst.tif.calc.qkm <- list.files(path.tif.calc, pattern = paste("QKM", tmp.date, sep = ".*"), full.names = TRUE)
#   
#   ## Import listed tifs as raster images
#   lst.rst.1km <- lapply(lst.tif.calc.1km, raster)
#   lst.rst.hkm <- lapply(lst.tif.calc.hkm, raster)
#   lst.rst.qkm <- lapply(lst.tif.calc.qkm, raster)
#   
#   
#   ## Get pixel matrix indices for 1km resolution
#   cells.1km <- cellFromXY(lst.rst.1km[[1]], data.bio.sp[b, ])
#   cells.adj.1km <- adjacent(lst.rst.1km[[1]], cells.1km, 
#                             directions = 8, 
#                             pairs = FALSE, 
#                             sorted = TRUE)
#   
#   ## Get pixel matrix indices for 500m resolution
#   cells.hkm <- cellFromXY(lst.rst.hkm[[1]], data.bio.sp[b, ])
#   cells.adj.hkm <- adjacent(lst.rst.hkm[[1]], cells.hkm, 
#                             directions = matrix(c(1,1,1,1,1,
#                                                   1,1,1,1,1,
#                                                   1,1,0,1,1,
#                                                   1,1,1,1,1,
#                                                   1,1,1,1,1), 
#                                                 ncol = 5), 
#                             pairs = FALSE, 
#                             sorted = TRUE)
#   
#   ## Get pixel matrix indices for 250m resolution
#   cells.qkm <- cellFromXY(lst.rst.qkm[[1]], data.bio.sp[b, ])
#   cells.adj.qkm <- adjacent(lst.rst.qkm[[1]], cells.qkm, 
#                             directions = matrix(c(1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,0,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1,
#                                                   1,1,1,1,1,1,1,1,1,1,1), 
#                                                 ncol = 11),  
#                             pairs = FALSE, 
#                             sorted = TRUE)
#   
#   
#   ## Calculate matrix standard deviation for each 1km raster image
#   pxl.rst.1km <- foreach(r = seq(lst.rst.1km), .combine = "cbind") %do% {
#     cells.1km.sd <- sd(lst.rst.1km[[r]][cells.adj.1km], na.rm = TRUE)
#     return(cells.1km.sd)
#   }
#   
#   ## Calculate matrix standard deviation for each 500m raster image
#   pxl.rst.hkm <- foreach(r = seq(lst.rst.hkm), .combine = "cbind") %do% {
#     cells.hkm.sd <- sd(lst.rst.hkm[[r]][cells.adj.hkm], na.rm = TRUE)
#     return(cells.hkm.sd)
#   }
#   
#   ## Calculate matrix standard deviation for each 250m raster image
#   pxl.rst.qkm <- foreach(r = seq(lst.rst.qkm), .combine = "cbind") %do% {
#     cells.qkm.sd <- sd(lst.rst.qkm[[r]][cells.adj.qkm], na.rm = TRUE)
#     return(cells.qkm.sd)
#   }
#   
#   ## Combine calculated standard deviation in a single row
#   row.sd <- cbind(pxl.rst.qkm, pxl.rst.hkm, pxl.rst.1km)
#   return(row.sd)
# }
# stopCluster(cl)
# 
# ## Write calculated values to new dataframe
# matrix.sd <- data.frame(matrix.sd)
# 
# ## Set names of matrix.sd dataframe
# names(matrix.sd) <- foreach(i = bandnames, j = names(matrix.sd)) %do% {
#   j <- paste0("sd_band_", i)
# }
# 
# names(matrix.sd)


### Combine dataframes #########################################################

df.greyvalues <- data.frame(cbind(data.bio.raw, 
                                       greyvalues), 
                                 stringsAsFactors = FALSE)

write.table(df.greyvalues, 
            file = path.biodiversity.csv.out,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


### Runtime calculation ########################################################

endtime <- Sys.time()
time <- endtime - starttime
time

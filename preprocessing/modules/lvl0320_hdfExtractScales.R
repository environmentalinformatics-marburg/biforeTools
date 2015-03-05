################################################################################
##  
##  BiFoRe Scripts
##    
##  Extract Radiance-/ Reflectance scales for each MODIS band from *.hdf files 
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

hdfExtractMODScale <- function(lst.hdf.qkm,
                               lst.hdf.hkm,
                               lst.hdf.1km
)
{
  
  ## Required packages
  stopifnot(require(rgdal))
  
  
  ## GDALinfo from HDF
  info.250.hdf <- GDALinfo(lst.hdf.qkm, returnScaleOffset = F)
  info.500.hdf <- GDALinfo(lst.hdf.hkm, returnScaleOffset = F)
  info.1km.hdf <- GDALinfo(lst.hdf.1km, returnScaleOffset = F)
  
  
  ## HDF attributes
  #attributes(info.250.hdf) # display attributes
  subds.250 <- attr(info.250.hdf, "subdsmdata")  # display subdatasets
  subds.500 <- attr(info.500.hdf, "subdsmdata")  # display subdatasets
  subds.1km <- attr(info.1km.hdf, "subdsmdata")  # display subdatasets
  
  
  ## Identify relevant SubDS via regular expression
  refsb.250 <- subds.250[grep("EOS_SWATH.*EV_250_RefSB$", subds.250)]
  refsb.500 <- subds.500[grep("EOS_SWATH.*EV_500_RefSB$", subds.500)]
  refsb.1km <- subds.1km[grep("EOS_SWATH.*EV_1KM_RefSB$", subds.1km)]
  emiss.1km <- subds.1km[grep("EOS_SWATH.*EV_1KM_Emissive$", subds.1km)]
  
  
  ## Remove irrelevant parts of SubDS name
  refsb.250 <- unlist(strsplit(refsb.250, "="))[2]
  refsb.500 <- unlist(strsplit(refsb.500, "="))[2]
  refsb.1km <- unlist(strsplit(refsb.1km, "="))[2]
  emiss.1km <- unlist(strsplit(emiss.1km, "="))[2]
  
  
  ## GDALinfo from SubDS
  info.subds.refsb.250 <- GDALinfo(refsb.250)
  info.subds.refsb.500 <- GDALinfo(refsb.500)
  info.subds.refsb.1km <- GDALinfo(refsb.1km)
  info.subds.emiss.1km <- GDALinfo(emiss.1km)
  
  
  ## Extract SubDS metadata
  mdata.subds.refsb.250 <- attr(info.subds.refsb.250, "mdata")
  mdata.subds.refsb.500 <- attr(info.subds.refsb.500, "mdata")
  mdata.subds.refsb.1km <- attr(info.subds.refsb.1km, "mdata")
  mdata.subds.emiss.1km <- attr(info.subds.emiss.1km, "mdata")
  
  ## Extract radiance-/reflectance_scales from metadata
  scales.refsb.250 <- mdata.subds.refsb.250[grep("reflectance_scales", mdata.subds.refsb.250)]
  scales.refsb.500 <- mdata.subds.refsb.500[grep("reflectance_scales", mdata.subds.refsb.500)]
  scales.refsb.1km <- mdata.subds.refsb.1km[grep("reflectance_scales", mdata.subds.refsb.1km)]
  scales.emiss.1km <- mdata.subds.emiss.1km[grep("radiance_scales", mdata.subds.emiss.1km)]
  
  scales.refsb.250 <- unlist(strsplit(scales.refsb.250, "="))[2]
  scales.refsb.500 <- unlist(strsplit(scales.refsb.500, "="))[2]
  scales.refsb.1km <- unlist(strsplit(scales.refsb.1km, "="))[2]
  scales.emiss.1km <- unlist(strsplit(scales.emiss.1km, "="))[2]
  
  ## write extracted values as numeric list
  sapply(strsplit(scales.refsb.250, ", "), as.numeric)
  sapply(strsplit(scales.refsb.500, ", "), as.numeric)
  sapply(strsplit(scales.refsb.1km, ", "), as.numeric)
  sapply(strsplit(scales.emiss.1km, ", "), as.numeric)
  
  ## Extract bandnames from metadata
  bands.refsb.250 <- mdata.subds.refsb.250[grep("band_names", mdata.subds.refsb.250)]
  bands.refsb.500 <- mdata.subds.refsb.500[grep("band_names", mdata.subds.refsb.500)]
  bands.refsb.1km <- mdata.subds.refsb.1km[grep("band_names", mdata.subds.refsb.1km)]
  bands.emiss.1km <- mdata.subds.emiss.1km[grep("band_names", mdata.subds.emiss.1km)]
  
  bands.refsb.250 <- unlist(strsplit(bands.refsb.250, "="))[2]
  bands.refsb.500 <- unlist(strsplit(bands.refsb.500, "="))[2]
  bands.refsb.1km <- unlist(strsplit(bands.refsb.1km, "="))[2]
  bands.emiss.1km <- unlist(strsplit(bands.emiss.1km, "="))[2]
  
  ## Exctract Offset from metadata
  offset.refsb.250 <- mdata.subds.refsb.250[grep("reflectance_offsets", mdata.subds.refsb.250)]
  offset.refsb.500 <- mdata.subds.refsb.500[grep("reflectance_offsets", mdata.subds.refsb.500)]
  offset.refsb.1km <- mdata.subds.refsb.1km[grep("reflectance_offsets", mdata.subds.refsb.1km)]
  offset.emiss.1km <- mdata.subds.emiss.1km[grep("radiance_offsets", mdata.subds.emiss.1km)]
  
  offset.refsb.250 <- unlist(strsplit(offset.refsb.250, "="))[2]
  offset.refsb.500 <- unlist(strsplit(offset.refsb.500, "="))[2]
  offset.refsb.1km <- unlist(strsplit(offset.refsb.1km, "="))[2]
  offset.emiss.1km <- unlist(strsplit(offset.emiss.1km, "="))[2]
  
  ## write extracted values as numeric list
  sapply(strsplit(offset.refsb.250, ", "), as.numeric)
  sapply(strsplit(offset.refsb.500, ", "), as.numeric)
  sapply(strsplit(offset.refsb.1km, ", "), as.numeric)
  sapply(strsplit(offset.emiss.1km, ", "), as.numeric)
  
  ### Write Modis bandnames and radiance scales to a single dataframe ##########
  
  ## paste bandnames and radiance scale
  bandnames <- paste(bands.refsb.250, 
                     bands.refsb.500, 
                     bands.refsb.1km,
                     bands.emiss.1km,
                     sep = ",")
  
  scales <- paste(scales.refsb.250,
                  scales.refsb.500,
                  scales.refsb.1km,
                  scales.emiss.1km,
                  sep = ", ")
  
  offsets <- paste(offset.refsb.250,
                   offset.refsb.500,
                   offset.refsb.1km,
                   offset.emiss.1km,
                   sep = ", ")
  
  
  ## Write bandnames, radiance scales and offsets to separate dataframe
  bandnames <- data.frame(strsplit(unlist(bandnames), ","), 
                          stringsAsFactors = F)
  names(bandnames) <- "bands"
  
  scales <- data.frame(strsplit(unlist(scales), ", "), 
                       stringsAsFactors = F)
  names(scales) <- "scales"
  
  offsets <- data.frame(strsplit(unlist(offsets), ", "), 
                        stringsAsFactors = F)
  names(offsets) <- "offsets"
  
  
  ## Rename "hi" and "lo" bands to numeric values
  for (i in seq(nrow(bandnames))) {
    if (bandnames[i,1] == "13lo")
      bandnames[i,1] <- "13.1"
    else if (bandnames[i,1] == "13hi")
      bandnames[i,1] <- "13.2"
    else if (bandnames[i,1] == "14lo")
      bandnames[i,] <- "14.1"
    else if (bandnames[i,1] == "14hi")
      bandnames[i,1] <- "14.2"
  }
  bandnames[,1] <- as.numeric(bandnames[,1])
  
  
  ## Write bandnames and radiance scales to a single dataframe
  modscales <- data.frame(cbind(bandnames, scales, offsets))
  
  ## Order data frame
  modscales <- modscales[ order(modscales[,1]), ]
  row.names(modscales) <- NULL
  
  ## return extracted values to call-script
  return(modscales)
}

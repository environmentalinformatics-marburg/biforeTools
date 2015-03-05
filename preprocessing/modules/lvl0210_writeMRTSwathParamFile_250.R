################################################################################
##  
##  BiFoRe Scripts
##    
##  FUNCTION: Write Parameterfile for MRTSwath Tool (250m resolution)
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

writeMRTSwathParamFile_250 <- function(prmfn = "tmpMRTparams.prm", 
                                       tifsdir, 
                                       modfn, geoloc_fn, 
                                       sds = NULL, 
                                       ul_lon, ul_lat, lr_lon, lr_lat) {
  
  ## Initialize the list of lines in the parameter file
  prmfile = NULL
  pnum = 0
  prmfile[[(pnum = pnum + 1)]] =   " "
  
  ## Input files
  prmfile[[(pnum = pnum + 1)]] =   paste("INPUT_FILENAME = ", modfn, sep = "")
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	paste("GEOLOCATION_FILENAME = ", geoloc_fn, sep = "")
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] =  "INPUT_SDS_NAME = EV_250_RefSB, 1, 1"  
  
  ## Subset parameters
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	paste("OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG", sep = "")
  prmfile[[(pnum = pnum + 1)]] = 	paste("OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) =", ul_lon, ul_lat, sep = " ")
  prmfile[[(pnum = pnum + 1)]] = 	paste("OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ", lr_lon, lr_lat, sep = " ")
  
  ## Output filename
  prmfile[[(pnum = pnum + 1)]] = 	" "
  
  outfn = gsub(pattern = ".hdf", replacement = ".tif", modfn)
  outfn = extract_fn_from_path(fn_with_path = outfn)
  outfn = slashslash(paste(tifsdir, outfn, sep = "/"))
  
  prmfile[[(pnum = pnum + 1)]] = 	paste("OUTPUT_FILENAME = ", outfn, sep="")
  prmfile[[(pnum = pnum + 1)]] = 	paste("OUTPUT_FILE_FORMAT = GEOTIFF_FMT", sep="")
  
  ## Reprojection information (for Geographic Projection, with nearest-neighbor resampling)
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	"KERNEL_TYPE (CC/BI/NN) = NN"
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	"OUTPUT_PROJECTION_NUMBER = GEO"
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	"OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"
  prmfile[[(pnum = pnum + 1)]] = 	" "
  prmfile[[(pnum = pnum + 1)]] = 	"OUTPUT_PROJECTION_SPHERE = 8"
  prmfile[[(pnum = pnum + 1)]] = 	" "
  
  ## Write the MRTSwath tool parameter file
  write.table(x = prmfile, 
              file = prmfn, 
              append = FALSE, 
              quote = FALSE, 
              sep = "\n", 
              row.names = FALSE, 
              col.names = FALSE)
  
  return(prmfn)
}

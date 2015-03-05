################################################################################
##  
##  BiFoRe Scripts
##    
##  FUNCTION: Run MRTSwath Tool 
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

runSwath2Grid <- function(mrtpath = "swath2grid", 
                          prmfn = "tmpMRTparams.prm", 
                          tifsdir = ".", 
                          modfn, 
                          geoloc_fn, 
                          ul_lon, ul_lat, lr_lon, lr_lat) {
  
  which_result = system(paste("which ", mrtpath, sep = ""), 
                        intern = TRUE)
  if (length(which_result) == 0) {
    return(paste("Error: mrtpath (", mrtpath, ") does not correspond to a file. Having swath2grid from MRTswatch is required for this function.", 
                 sep = ""))
  }

  cmdstr = paste(mrtpath, " -pf=", prmfn, sep = "")
  system(cmdstr)
  
  return(cmdstr)
}

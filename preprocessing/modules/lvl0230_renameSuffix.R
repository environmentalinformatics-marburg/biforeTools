################################################################################
##  
##  BiFoRe Scripts
##    
##  Rename MODIS files 
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

renameSuffix <- function(files, 
                         suffix.in, 
                         suffix.out, 
                         tifsdir) {
  
  index <- grep(suffix.in, files)
  
  file.in <- as.list(files[index])
  
  foreach (k = file.in) %do% {
    file.out <- substr(basename(k), 1, 22)
    file.out <- paste0(tifsdir, suffix.out, "_", file.out, ".tif")
    
    file.rename(k, file.out)
  }
}

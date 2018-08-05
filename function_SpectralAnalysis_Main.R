## MAIN LINE FUNCTION:  integrating all the important script to
##   conduct spectral analysis anywhere.
## 

##  necessary libraries
library(ncdf4)
library(raster)
library(maps)
library(rgdal)


## subroutines
#FunctionSourceDir = '/Users/ljr/Workspace/RainfallSpectralAnalysis/SpectralAnalysisScripts/'
FunctionSourceDir = '/g/data1a/fj4/users/renzullo/SpectralAnalysisScripts/'

source(paste0(FunctionSourceDir,'function_ll2aea.R'))  ## converts lat longs in to AEA-Aus.
source(paste0(FunctionSourceDir,'function_isopsd.R'))  ## ISO PSD calculation
source(paste0(FunctionSourceDir,'function_ps2d.R'))    ## 2D fourier power spectrum
source(paste0(FunctionSourceDir,'function_rainfallVerificationStats.R'))  ## verification stats
source(paste0(FunctionSourceDir,'function_SetupForGraphics.R'))   ##  functions for graphics



#  DEFINES A SQUARE AREA (in Eastings and Northings) ABOUT A GIVEN (lat,lon) POINT.
DefineArea = function(lat, lon, L) {
   # - lat :   single value latitude of the site of interest  (degrees North)
   # - lon :   single value longitude of the site of interest (degrees East)
   # - L  :    length of the size of the square areas (extent of the square length of the (m) 
   #
  
    cLL = SpatialPoints(cbind(lon,lat),proj4string=CRS(PROJ_SOURCE))
    cNE = spTransform(cLL,CRS(PROJ_TARGET))
    Easting_center  = as.vector(cNE@coords)[1]  # in meters (m)
    Northing_center = as.vector(cNE@coords)[2]
    Easting_min     = Easting_center - L / 2.
    Easting_max     = Easting_min + L
    Northing_max    = Northing_center + L / 2.
    Northing_min    = Northing_max - L
    
    return(list(Ec  = Easting_center, 
                Nc  = Northing_center,
                Emn = Easting_min,  Emx = Easting_max,
                Nmn = Northing_min, Nmx = Northing_max ))
}





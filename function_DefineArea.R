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


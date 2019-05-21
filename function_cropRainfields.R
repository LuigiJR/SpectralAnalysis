##  function that gives a cropped raster for a geographic subset
##   of rainfall grid with projection consistent with that of 
##   Rainfields_v3.0

cropRainfields = function(Lat,Long,L=512,RainfieldsRaster) {
 #
 #  L is in pixels ... assumed square region of interest
 #
 
  dN = L/2

  source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_ll2aea.R')
  
  proj  = '+proj=aea +lat_1=-18.0 +lat_2=-36.0 +lon_0=132 +lat_0=0 +datum=WGS84'
  nrows = 2050; ncols = 2450; dx = 2000 #dx in m
  Eastings = seq(-2299000,2599000,dx); Northings = seq(-5099000,-1001000,dx)
  Xmn = -2300000; Xmx = 2600000; Ymn = -5100000; Ymx = -1000000

  lat_oi = Lat; lon_oi = Long
    en_oi  = ll2aea(lat_oi,lon_oi)
    e_oi   = en_oi$E; n_oi = en_oi$N

   col_oi = which.min((Eastings  - e_oi)**2)
   row_oi = which.min((rev(Northings) - n_oi)**2)

   cropped_rst = crop(RainfieldsRaster,
		      extent(RainfieldsRaster,row_oi-(dN-1),row_oi+dN,
			                      col_oi-(dN-1),col_oi+dN))
 return(cropped_rst)
}

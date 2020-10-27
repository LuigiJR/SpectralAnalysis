getCellfromLocation = function(Lat,Long,Raster) {
	# Raster must be a raster object
	projinfo = as.character(crs(Raster))
	ProjLatLon = '+proj=longlat +datum=WGS84'
		ll = SpatialPoints(cbind(Long,Lat),proj4string=CRS(ProjLatLon))
		en = spTransform(ll,CRS(projinfo))
	return(cellFromXY(Raster,en))
}

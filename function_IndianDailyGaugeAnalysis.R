IndianDailyGaugeAnalysis = function (DOY,path2data) {
	## DOY = Day of Year, expressed as as.Date('2020-06-02') 

	NROWS  = 129; NCOLS = 135
	Null = -999
	
       	W_Lon = 66.375; E_Lon = W_Lon + (NCOLS)*0.25
       	S_Lat  = 6.375; N_Lat = S_Lat + (NROWS)*0.25



	fl = paste0(path2data,format(DOY,'/%Y%m/'),'rain_ind0.25_',format(DOY,'%y_%m_%d'),'.grd')
	x = readBin(fl,'numeric',size=4,n=NROWS*NCOLS)
	x = matrix(x,NROWS,NCOLS,byrow=TRUE)[NROWS:1,]; x[x<0] = NA
	x_rst = raster(x,crs=PROJ_LATLON,W_Lon,E_Lon,S_Lat,N_Lat)

	return(x_rst)

}

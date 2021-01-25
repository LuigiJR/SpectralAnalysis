
get60min_acc_ncum_r = function(path2data,ValidDateTime) {

	##  function to provide the NCUM-R filename and accumulated precip
        ##  for a give VALID in data & time.
	##  NOTE that the path2data needs to be updated to relevant systems

	filename_ncum_r = function(path2data,validDateTime) {
        
		validDateTime = validDateTime - 1. ## to avoid issue with accum to 12 am.
		fln = paste0(path2data,format(validDateTime,'%Y%m%/'),
			    'combine_time_model_rain_',format(validDateTime,'%Y%m%d.nc'))

	        return(fln)
	}

   proj = '+proj=longlat +datum=WGS84'
   Lons_min = 61.98;  Lons_max = 106.02
   Lats_min =-0.020;  Lats_max = 40.02
	nrows = 1001

     vdatetime1 = ValidDateTime

	file_r  = filename_ncum_r(path2data,ValidDateTime)

	if (!file.exists(file_r)) {
                return(paste0('----- No acc. possible for ',ValidDateTime))
	} else {

	xo = nc_open(file_r)
#		endtimes = ISOdatetime(1970,1,1,0,0,0,tz='GMT') + # second since... 
# 				ncvar_get(xo,'time')[c(1,10:19,2,20:24,3:9)]

               endtimes = ISOdatetime(1970,1,1,0,0,0,tz='GMT') + # second since... 
                               ncvar_get(xo,'time')

	which_t = which(endtimes == ValidDateTime)	
	
	x = t(ncvar_get(xo,'APCP_surface',start=c(1,1,which_t),count=c(-1,-1,1)))[nrows:1,]
	x_rst = raster(x,crs=proj,Lons_min,Lons_max,Lats_min,Lats_max)
	return(x_rst)
	}
}

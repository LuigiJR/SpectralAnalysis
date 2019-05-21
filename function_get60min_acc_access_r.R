
get60min_acc_access_r = function(path2data,ValidDateTime) {

	##  function to provide the ACCESS-R filename and accumulated precip
        ##  for a give VALID in data & time.
	##  NOTE that the path2data needs to be updated to relevant systems

	filename_access = function(path2data,validDateTime) {
        
        	HRS  = as.numeric(format(validDateTime,'%H'))
        
	        if (as.integer((HRS+2)/6)==0) {
        	        RunTime = "1800" 
                	RunDate = format(validDateTime - 24*3600,'%Y%m%d/')
	        }
        	if (as.integer((HRS+2)/6)==1) {
                	RunTime = "0000"
	                RunDate = format(validDateTime,'%Y%m%d/')
        	}
	        if (as.integer((HRS+2)/6)==2) {
        	        RunTime = "0600"
                	RunDate = format(validDateTime,'%Y%m%d/')
	        }
        	if (as.integer((HRS+2)/6)==3) {
                	RunTime = "1200"
	                RunDate = format(validDateTime,'%Y%m%d/')
        	}
               
               RunDir  = paste0(path2data,
                                 RunDate,
                                 RunTime,'/')
         	SubDirs = 'fc/sfc/'
	         File    = 'accum_prcp.nc'
        	fname = paste0(RunDir,SubDirs,File)

	        return(fname)
	}

   proj = '+proj=longlat +datum=WGS84'
   Lons_min = 64.945;  Lons_max=184.625
   Lats_min =-65.055;  Lats_max=17.005

     vdatetime1 = ValidDateTime
     vdatetime2 = ValidDateTime - 60*60  # 60 min previous

	file_r  = filename_access(path2data,ValidDateTime)

	if (!file.exists(file_r)) {
                return(paste0('----- No acc. possible for ',ValidDateTime))
	} else {

	xo = nc_open(file_r)
		basetime = as.character(ncvar_get(xo,'base_time')[1])
		basedate = as.character(ncvar_get(xo,'base_date')[1])

		iso_basetime = ISOdatetime(substr(basedate,1,4),
					   substr(basedate,5,6),
					   substr(basedate,7,8),
					   substr(basetime,1,2),0,0,tz='GMT')
		f_times = ncvar_get(xo,'time')	# in  seconds

		all_validTimes = iso_basetime + f_times

		k_1 = which(all_validTimes==vdatetime1)	
		k_2 = which(all_validTimes==vdatetime2)	
	
		x_1 = ncvar_get(xo,'accum_prcp',start=c(1,1,k_1),count=c(-1,-1,1))
		x_2 = ncvar_get(xo,'accum_prcp',start=c(1,1,k_2),count=c(-1,-1,1))

		x = t(x_1 - x_2)

		rm(x_1,x_2)
	nc_close(xo)

	x_rst = raster(x,crs=proj,Lons_min,Lons_max,Lats_min,Lats_max)
	return(x_rst)
	}
}

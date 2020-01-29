
##  function to provide the gpm filename for a give VALID in data & time
##  NOTE that the path2data needs to be updated to relevant systems

filename_gpm = function(path2data,validDateTime) {

	validDateTime = validDateTime -1. # to deal with 0000UTC date issue

        YYYY = format(validDateTime,'%Y')
        MM   = format(validDateTime,'%m')
        DD   = format(validDateTime,'%d')

        YYYYMM = paste0(YYYY,MM,'/')
        stem = '3B-HHR-L.MS.MRG.3IMERG.'
        yyyymmdd = format(validDateTime,'%Y%m%d-')
        Stime = validDateTime - 30*60 + 1  # makes the start 00 or 30
        Stext = format(Stime,'S%H%M%S-')
        Etime = validDateTime 
        Etext = format(Etime,'E%H%M%S.')
        Interval = difftime(Stime,ISOdatetime(YYYY,MM,DD,0,0,0,tz='GMT'),units='secs') 
        IntervalText = sprintf('%04d.',Interval/60.)
        leaf  = 'V06B.RT-H5'

        fname = paste0(path2data,YYYYMM,stem,yyyymmdd,Stext,Etext,IntervalText,leaf)

        return(fname)
}

##   function to return the GPM equivalent of 60-minute accumulated 
##    rainfall by adding two consectutive 30-min rain rates


get60min_acc_gpm = function(path2data,ValidDateTime){

   proj = '+proj=longlat +datum=WGS84'
   Lons_min = -180.;Lons_max=180.
   Lats_min=-90.;   Lats_max=90.

	vdatetime1 = ValidDateTime
	vdatetime2 = ValidDateTime - 30 * 60  # 30 min previous

	f1  = filename_gpm(path2data,vdatetime1)
	f2  = filename_gpm(path2data,vdatetime2)
	f1_exists = file.exists(f1)
	f2_exists = file.exists(f2)

	if (!f1_exists & !f2_exists) {
		return(paste0('----- No acc. possible for ',ValidDateTime))
	} else {

		if ( f1_exists & f2_exists ) {
			xo = nc_open(f1)
				x_1 = ncvar_get(xo,'Grid/precipitationCal')
				x_1 = x_1[dim(x_1)[1]:1,]
			nc_close(xo)
                        xo = nc_open(f2)
                           dx = 0.1
                           	x_2 = ncvar_get(xo,'Grid/precipitationCal')
                           	x_2 = x_2[dim(x_2)[1]:1,]
                        nc_close(xo)

			x = 0.5 * ( x_1 + x_2 )

			rm(x_1,x_2)
		} else {
			fl  = c(f1,f2)
			f0 = fl[which(c(f1_exists,f2_exists))]
			xo = nc_open(f0)
                           x = ncvar_get(xo,'Grid/precipitationCal')
                           x = x[dim(x)[1]:1,]
                        nc_close(xo)
		}
		x_rst = raster(x,crs=proj,xmn=Lons_min,xmx=Lons_max,
                                          ymn=Lats_min,ymx=Lats_max)	
	}	
 return(x_rst)
}


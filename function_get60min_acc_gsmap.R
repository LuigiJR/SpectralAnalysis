
##  function to provide the gpm filename for a give VALID in data & time
##  NOTE that the path2data needs to be updated to relevant systems

filename_gsmap = function(path2data,validDateTime) {

	validDateTime = validDateTime -1. # to deal with 0000UTC date issue

        YYYY = format(validDateTime,'%Y')
        MM   = format(validDateTime,'%m')
        DD   = format(validDateTime,'%d')

	JDAY = format(validDateTime,'%j')

        path2data=paste0(path2data,YYYY,'/',JDAY,'/')
        stem = 'gsmap_now.'
        yyyymmdd = format(validDateTime,'%Y%m%d.')
        hhmm = format(validDateTime + 1,'%H%M')
        leaf  = '_global.nc'

        fname = paste0(path2data,stem,yyyymmdd,hhmm,leaf)

        return(fname)
}

##   function to return the GSMaP equivalent of 60-minute accumulated 
##    rainfall by adding two consectutive 30-min rain rates


get60min_acc_gsmap = function(path2data,ValidDateTime){

   proj = '+proj=longlat +datum=WGS84'
   Lons_min = 0.;Lons_max=360.
   Lats_min=-60.;   Lats_max=60.

	vdatetime1 = ValidDateTime
	vdatetime2 = ValidDateTime - 30 * 60  # 30 min previous

	f1  = filename_gsmap(path2data,vdatetime1)
	f2  = filename_gsmap(path2data,vdatetime2)
	f1_exists = file.exists(f1)
	f2_exists = file.exists(f2)

	if (!f1_exists & !f2_exists) {
		return(paste0('----- No acc. possible for ',ValidDateTime))
	} else {

		if ( f1_exists & f2_exists ) {
			xo = nc_open(f1)
				x_1 = t(ncvar_get(xo,'RainRate'))
			nc_close(xo)
                        xo = nc_open(f2)
                           	x_2 = t(ncvar_get(xo,'RainRate'))
                        nc_close(xo)

			x = 0.5 * ( x_1 + x_2 )

			rm(x_1,x_2)
		} else {
			fl  = c(f1,f2)
			f0 = fl[which(c(f1_exists,f2_exists))]
			xo = nc_open(f0)
                           x = t(ncvar_get(xo,'RainRate'))
                        nc_close(xo)
		}
		x_rst = raster(x,crs=proj,xmn=Lons_min,xmx=Lons_max,
                                          ymn=Lats_min,ymx=Lats_max)	
	}	
 return(x_rst)
}


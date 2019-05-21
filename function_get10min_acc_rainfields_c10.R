
##   function to return the GPM equivalent of 60-minute accumulated 
##    rainfall by adding two consectutive 30-min accumulated products 

get10min_acc_rainfields_c10 = function(RainfieldsDir,ValidDateTime){


	##  function to provide the gpm filename for a give VALID in data & time
	##  NOTE that the path2data needs to be updated to relevant systems

	filename_rainfields10min = function(path2data,validDateTime) {

        	YYYY = format(validDateTime,'%Y')
	        MM   = format(validDateTime,'%m')
	        DD   = format(validDateTime,'%d')

        	YYYYMMDD = paste0(YYYY,MM,DD,'_')
	        stem = '310_'
	        Etime = validDateTime
	        Etext = format(Etime,'%H%M%S.')
	        leaf  = 'prcp-c10.nc'

        	fname = paste0(path2data,'/',YYYY,'/',MM,'/',DD,'/',
                	       stem,YYYYMMDD,Etext,leaf)

	        return(fname)
	}


  proj  = '+proj=aea +lat_1=-18.0 +lat_2=-36.0 +lon_0=132 +lat_0=0 +datum=WGS84'
  nrows = 2050; ncols = 2450; dx = 2000 #dx in m
  Eastings = seq(-2299000,2599000,dx); Northings = seq(-5099000,-1001000,dx)
  Xmn = -2300000; Xmx = 2600000; Ymn = -5100000; Ymx = -1000000

        vdatetime_x = ValidDateTime 
                                   
	fl_x = filename_rainfields10min(RainfieldsDir,vdatetime_x)
		
	if (file.exists(fl_x)) {
                        xo = nc_open(fl_x)
                        x_1 = t(ncvar_get(xo,'precipitation'))   ## -- mm in 10 minutes
                        nc_close(xo)
                        x_vec = x_1
	} else {
			x_vec = rep(NA,nrows*ncols)
	}
	
   x_rst = raster(matrix(x_vec,nrows,ncols),crs=proj,xmn=Xmn,xmx=Xmx,ymn=Ymn,ymx=Ymx)
   return(x_rst)
}


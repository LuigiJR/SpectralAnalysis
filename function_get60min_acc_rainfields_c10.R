
##   function to return the GPM equivalent of 60-minute accumulated 
##    rainfall by adding two consectutive 30-min accumulated products 

get60min_acc_rainfields_c10 = function(RainfieldsDir,ValidDateTime){

	##  function to provide the gpm filename for a give VALID in data & time
	##  NOTE that the path2data needs to be updated to relevant systems

	filename_rainfields = function(path2data,validDateTime) {

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

        vdatetime_x = ValidDateTime - 10 * 60 * (0:5)  # 60 min previous in
                                                       # 10 min increments
	valid_files = c() 
	for (j in 1:length(vdatetime_x)) {
		fl_x = filename_rainfields(RainfieldsDir,vdatetime_x[j])
		
		if (file.exists(fl_x)) {
			valid_files = c(valid_files,fl_x)
		}
	}
	
	Nfiles = length(valid_files)
	
	if (Nfiles > 0) {
		x_vec = c()
		for (i in 1:Nfiles) {
                        xo = nc_open(valid_files[i])
                        x_1 = t(ncvar_get(xo,'precipitation'))   ## -- mm in 10 minutes
                        nc_close(xo)
			x_vec = rbind(x_vec,as.vector(x_1))
		}
		x_vec = colMeans(x_vec,na.rm=TRUE)*6.0  # *6 to get 60 minute
	} else {
		x_vec = rep(NA,nrows*ncols) 
	}

   x_rst = raster(matrix(x_vec,nrows,ncols),crs=proj,xmn=Xmn,xmx=Xmx,ymn=Ymn,ymx=Ymx)
   return(x_rst)
}


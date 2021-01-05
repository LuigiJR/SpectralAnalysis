
##  function to provide the insat HydroEstimator filename for a give VALID in data & time
##  NOTE that the path2data needs to be updated to relevant systems

filename_insat3dr = function(path2data,DateTime) {

        YYYY = format(DateTime,'%Y')
        MON   = toupper(format(DateTime,'%b'))
        MM   = format(DateTime,'%m')
        DD   = format(DateTime,'%d')

        YYYYMM = paste0(YYYY,MM,DD,'/')
        stem  = paste0('3RIMG_',DD,MON,YYYY,'_')
        hrmin = format(DateTime,'%H%M_')
        leaf  = 'L2B_HEM.h5'

        fname = paste0(path2data,YYYYMM,stem,hrmin,leaf)

        return(fname)
}

##   function to return the INSAT equivalent of 60-minute accumulated 
##    rainfall by adding two consectutive 30-min rain rates


get60min_acc_insat3dr = function(path2data,ValidDateTime){
###  INSAT data
        vROWS = 58:2760
        vCOLS = 48:2758
	proj = '+proj=geos +lon_0=74.0 +h=36000000  +datum=WGS84'
        Emn = -5438829, Emx = 5438829, Nmn = -5420807, Nmx = 5420807 

	vdatetime1 = ValidDateTime - 45 * 60  # 45 min previous
	vdatetime2 = ValidDateTime - 15 * 60  # 15 min previous

	f1  = filename_insat3dr(path2data,vdatetime1)
	f2  = filename_insat3dr(path2data,vdatetime2)
	f1_exists = file.exists(f1)
	f2_exists = file.exists(f2)

	if (!f1_exists & !f2_exists) {
		print(paste0('----- No acc. possible for ',ValidDateTime))
		x_rst = raster(matrix(NA,length(vROWS),length(vCOLS)),crs=proj,xmn=Emn,xmx=Emx,ymn=Nmn,ymx=Nmx)
	} else {

		if ( f1_exists & f2_exists ) {
			xo = nc_open(f1)
				x_1 = t(ncvar_get(xo,'HEM'))[vROWS,vCOLS]
				x_1[x_1 <0] = NA
			nc_close(xo)
                        xo = nc_open(f2)
                           dx = 0.1
                           	x_2 = t(ncvar_get(xo,'HEM'))[vROWS,vCOLS]
				x_2[x_2 <0] = NA
                        nc_close(xo)

			x_mat = t(matrix(cbind(as.vector(x_1),as.vector(x_2))))
			x = matrix(colMeans(x_mat),dim(x_1)[1],dim(x_1)[2]); rm(x_mat)

			rm(x_1,x_2)
		} else {
			fl  = c(f1,f2)
			f0 = fl[which(c(f1_exists,f2_exists))]
			xo = nc_open(f0)
                           x = t(ncvar_get(xo,'HEM'))[vROWS,vCOLS]
				x[x<0] = NA
                        nc_close(xo)
		}
		x_rst = raster(x,crs=proj,xmn=Emn,xmx=Emx,ymn=Nmn,ymx=Nmx)	
	}	
 return(x_rst)
}


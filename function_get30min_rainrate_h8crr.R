
##  function to provide appropriate h8 filename for a give VALID data & time
##  NOTE that the path2data needs to be updated to relevant systems

filename_h8crr = function(path2data,validDateTime) {

        YYYY = format(validDateTime,'%Y')
        MM   = format(validDateTime,'%m')
        DD   = format(validDateTime,'%d')
	HR   = format(validDateTime,'%H%M')

	dirPath = paste0(path2data,YYYY,'/',MM,'/',DD,'/',HR,'/')

	stem  = paste0(YYYY,MM,DD,HR,'00')
        leaf  = '-P1S-ABOM_CRR-PRJ_AEA132_2000-HIMAWARI8-AHI.nc'

        fname = paste0(dirPath,stem,leaf)

        return(fname)
}

##   function to return the average rainfall rate over 30-minute period 
##    by adding consectutive H8-CRR 10-min rain rates


get30min_rainrate_h8crr = function(path2data,ValidDateTime){

   proj = '+proj=aea +lat_1=-18.0 +lat_2=-36.0 +lon_0=132 +lat_0=0 +datum=WGS84'
   E_min = -2300000 - 1000; E_max =  2598000 + 1000
   N_min = -5098000 - 1000; N_max = -1000000 + 1000

	vdatetime1 = ValidDateTime - 10*60
	vdatetime2 = ValidDateTime - 20*60
	vdatetime3 = ValidDateTime - 30*60

	f1  = filename_h8crr(path2data,vdatetime1)
	f2  = filename_h8crr(path2data,vdatetime2)
	f3  = filename_h8crr(path2data,vdatetime3)
	f1_exists = file.exists(f1)
	f2_exists = file.exists(f2)
	f3_exists = file.exists(f3)

	if (!f1_exists & !f2_exists & !f3_exists) {
		return(paste0('----- No acc. possible for ',ValidDateTime))
	} else {

		if (f1_exists & f2_exists & f3_exists) {
			xo1 = nc_open(f1)
			x1  = ncvar_get(xo1,'precipitation_flux')
			nc_close(xo1)
                        xo2 = nc_open(f2)
                        x2  = ncvar_get(xo2,'precipitation_flux')
                        nc_close(xo2)
                        xo3 = nc_open(f3)
                        x3  = ncvar_get(xo3,'precipitation_flux')
                        nc_close(xo3)
			
			x =  matrix(colMeans(
			     rbind(as.vector(x1),as.vector(x2),as.vector(x3))),
				    nrow(x1),ncol(x1))

		} else {
			fl = c(f1,f2,f3)[which(c(f1_exists,f2_exists,f3_exists))]
			x = c()
			for (ijk in 1:length(fl)) {
				xo1 = nc_open(fl[1])
				x1  = ncvar_get(xo1,'precipitation_flux')
				nc_close(xo1)
				x = rbind(x,as.vector(x1))	
			}
			
			x = matrix(colMeans(x),nrow(x1),ncol(x1))
		}
		x_rst = raster(t(x),crs=proj,E_min,E_max,N_min,N_max)
	}	
 return(x_rst)
}


# COMPUTES LOG/EXP TRANSFORM OF RAINFALL/LOG RAINFALL

logRain = function(R_in,inverse=FALSE,minRain = 0.1) {
  # - R_in    : is the image 2D array of rainfall/logRainfall, a raster 
  #                  or vector.
  # - inverse : deafault FALSE meaning log of R is calculated; 
  #                   if TRUE the exp(logRain) is computed.
  # - minRain : the rain/no rain threshold, default = 0.1 mm hr**-1
  #

	R_is_raster = is.object(R_in)
	if (R_is_raster) {
		r_template = R_in
		R_in = as.matrix(R_in)
	}

  isNA = which(is.na(R_in))
  logR = R_in
  if(!inverse) {
    isRain = round(logR,2) > minRain
    isNoRain = !isRain 
    logR[which(isNoRain)] = 0  
    logR[which(isRain)]  = log(R_in[which(isRain)]) - log(minRain)
    logR[isNA] = NA 
   R_out = logR 
  } else {
    logMinRain = log(minRain)
    isRain = logR > 0 
    isNoRain = !isRain
    R = exp(logR + logMinRain)
    R[which(isNoRain)] = 0
    R[isNA] = NA
   R_out = R  
  }

  if (R_is_raster) {R_out = raster(R_out,template=r_template)}
  return(R_out) 
}  

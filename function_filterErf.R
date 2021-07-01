filterErf = function(x,x_lower=NA,x_upper=NA) {
  #  Erf filter  (from my messing around with Erf - NormalCDF)
  #   x : vector of spatial frequencies
  #   x_lower :  lower limit for bandpass filter (normal cdf 1)  
  #   x_upper :  upper limit for bandpass filter (normal cdf 2)
  #   ss      :  sigma for the two normalcdf -10% of respective
  #                                        mean    	
  #
  #   filter = erf(x,x_lower) - erf(x,x_upper) then normalised 
  #                                            to be between 0-1
  #
  #    NB:  erf(x) = 2 pnorm(x,0,1) - 1
  #
	#both lower and upper specified
	if (!is.na(x_lower) & !is.na(x_upper)) { 
		
		g = pnorm(x,x_lower,0.1*x_lower) - pnorm(x,x_upper,0.1*x_upper)
	}
	if (is.na(x_lower)) { 
                g = -pnorm(x,x_upper,0.1*x_upper)
        } 
        if (is.na(x_upper)) {                   
                g = pnorm(x,x_lower,0.1*x_lower)
        }
	g = (g - min(g))/(max(g) - min(g))	
	return(g)
}

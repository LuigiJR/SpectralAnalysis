filterErf = function(x,x_lower,x_upper,ss) {
  #  Erf filter  (from my messing around with Erf - NormalCDF)
  #   x : vector of spatial frequencies
  #   x_lower :  lower limit for bandpass filter (normal cdf 1)  
  #   x_upper :  upper limit for bandpass filter (normal cdf 2)
  #   ss      :  sigma for the two normal cdf 
  #
  #   filter = erf(x,x_lower) - erf(x,x_upper) then normalised 
  #                                            to be between 0-1
  #
  #    NB:  erf(x) = 2 pnorm(x,0,1) - 1
  #

  	g = pnorm(x,x_lower,ss) - pnorm(x,x_upper,ss)
	g = (g - min(g))/(max(g) - min(g))	
	
	return(g)
}

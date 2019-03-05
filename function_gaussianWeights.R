
#  DERIVE GAUSSIAN WEIGHTS FOR THE FILTERING OF FOURIER TRANSFOR
#    --- these define the Cascade levels
#

gaussianWeights = function(nu_k, NC, dx, K=8, k) {

	# -- parameters:  
	L0 = 3.0 # - index to the position of the centre freq. of 2nd
	         #    cascade level.

	# -- functions:
	log_a = function(x,a) {
		res = -1.e+16 
		res[x > 0] = log(x[x > 0])/log(a)
		return(res)
	}	

	# - reciprocal of frwquency (or wavelength) in index space
	l_k = dx*NC* nu_k

	L = NC

	a = (L / (2. * L0))**(1./(K-2))
	b_k = 0.
	if (k >= 1) {
		b_k = log_a(L0,a) + k - 2
	} 
#	c_k = 0.3 * a
#	if (k > 0) {
#		c_k = 0.2 * a
#	}
        c_k = 0.5

	weights = exp(-(log_a(abs(l_k),a) - b_k)**2 / (2.*c_k**2))
	if (k ==1) {weights[1] = 1.}

        return(weights)
}




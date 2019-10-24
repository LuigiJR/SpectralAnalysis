#  Employ a simple mean field bias correction such that the 
#    mean for common coverage areas match.
#
mfbc = function(R_orig, R_targ) {
	#  -  R_orig  : the image we want to adjust, 
	#               likely the satellite data
	#  -  R_targ  : the image that serves as the 
	#               reference, likely the radar
	#
		
	Coverage = which(!is.na(R_targ))
	WAR0 = sum(R_targ[Coverage] > 0) / length(Coverage)
	TT0  = as.numeric(quantile(R_orig[Coverage],probs=1-WAR0))

	if (WAR0 > 0.0) {
		MM0 = mean(R_targ[Coverage][R_targ[Coverage] > 0])
		MMT = mean(R_orig[Coverage][R_orig[Coverage] > TT0])
			if (is.na(MMT)) MMT = 0

		R_star = R_orig - (MMT - MM0)
		T_star = TT0 - (MMT - MM0)
		R_star[R_star <= T_star] = 0
	} else {   # scenario when targ shows no wet pixels
		R_star = R_orig - TT0
		R_star[R_star <= 0] = 0
	}
return(R_star)
}


rainfallVerificationStats = function(R,G,zeroRth = 0.1) {
  #  R - the satellite, or other, estimate of rainfall; a vector
  #  G - the gauge or reference rainfall; a vector.
  #  zeroRth  - the rain / no rain threshold; scalar
  #
  #  NOTE:  assumes all NA's are exclued from the analysis.
  
  zero = zeroRth
	if (is.object(R)) {
		O = as.vector(as.matrix(G))
		E = as.vector(as.matrix(R))
	} else {
		O = G   #   reference, obs
		E = R   #   estimate
  	}
  a = length( which( E >= zero &  O >= zero ) ) #  HITS
  b = length( which( E >= zero &  O <  zero ) ) #  FALSE ALARMS
  c = length( which( E <  zero &  O >= zero ) ) #  MISSES
  d = length( which( E  < zero &  O <  zero ) ) #  CORRECT NEGATIVE
  
  a_ref = (as.numeric( a + b )*as.numeric( a + c ))  /( a + b + c + d )
  
  POD = a / ( a + c )
  FAR = b / ( a + b )
  ETS = ( a - a_ref ) / ( a + b + c - a_ref )
    
  D_AC = E  -  O
  RMSE = sqrt(mean(D_AC^2,na.rm=T))
  MAD  = mean(abs(D_AC),na.rm=T)
  BIAS = mean(D_AC,na.rm=T)
  COR  = cor(E,O)
  
  return(list=c(RMSE=RMSE, MAD=MAD,Bias=BIAS, Corr = COR,POD = POD, FAR=FAR, ETS = ETS))
}



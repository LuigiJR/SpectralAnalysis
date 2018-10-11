
# COMPUTES AND RETURNS THE FORWARD / INVERSE 2D DISCRETE FOURIER TRANSFORM
#    OF A 2D ARRAY 

dft2d = function(X,inverse=FALSE) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal. Note that in forward DFT
  #     X is a 2D array of reals (and function returns a 2D array of
  #     complex numbers, Y); whereas in inverse DFT X is a 2D array of 
  #     complex numbers (and function returns 'mostly' reals in Y)
  #
  #  - inverse : by default a forward DFT is return. if "inverse=T"
  #              the an inverse DFT is returned
  #

  NR = dim(X)[1]
  NC = NR
  
  # = = = =   Setting up for the Fourier analysis
  
  fft2d_rr = matrix(NA,NR,NC) # 2d Fourier transform
  xfft_rr  = matrix(NA,NR,NC) # temporary 2d array 


	if (!inverse) { 
		  ##### first fft (latitudinal direction)
		  for (i in 1:NR) {
		    xfft_rr[i,] = fft(X[i,])
		  }
		  ##   ' - - - - -  completed FT in x direction.'
		  #####  second fft in longitudinal direction to generate the 2D DFT
		  for (j in 1:NC) {
		    fft2d_rr[,j] = fft(xfft_rr[,j])
		  }
		  ##   ' - - - - -  completed FT in y direction.'
		  #
		  Y = fft2d_rr
	} else { 
                  ##### first fft (latitudinal direction)
                  for (i in 1:NR) {
                    xfft_rr[i,] = fft(X[i,], inverse = TRUE)  / NR
                  }
                  ##   ' - - - - -  completed FT in x direction.'
                  #####  second fft in longitudinal direction to generate the 2D DFT
                  for (j in 1:NC) {
                    fft2d_rr[,j] = fft(xfft_rr[,j], inverse = TRUE) / NR
                  }
                  ##   ' - - - - -  completed FT in y direction.'
                 #
		 Y = Re(fft2d_rr)
	}
	return(Y)
}


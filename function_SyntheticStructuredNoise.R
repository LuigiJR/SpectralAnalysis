
# CREATES A MAP OF SYNTHETIC NOISE WITH STRUCTURE FOLLOWING POWER LAW 

SyntheticStructuredNoise = function(NOISE,Beta) {
  # - NOISE  :  2D array of Gaussian white noise
	# dimensions NR, NC where
        # NR, NC  :  number of rows and columns over the areas of interets
  #              NOTE that these MUST be even numbers
  # - Beta :  power law exponent 
  #
  # Returns NR x NC 2D array of strctured noise

	NR =dim(NOISE)[1]; NC = NR

  # = = = =   Setting up for the Fourier analysis
  # - wavenumber ( pixel ** -1 ) 
  
	L  = NR
  	xc = c(0:(L/2),(L/2-1):1)
	yc = xc
	gg = expand.grid(xc,yc)
	
	A = matrix(sqrt(gg$Var1*gg$Var1+gg$Var2*gg$Var2),L,L)

  # - Power law filter
  H_k = A ** (-Beta/2.)
  H_k[!is.finite(H_k)] = 1. 
  
  # - Gaussian white noise
  NOISE_MATRIX = NOISE 

  fft2d_n = matrix(NA,NR,NC) # gaussian white noise 2d Fourier transform
  xfft_n  = matrix(NA,NR,NC) # temporary 2d array 

  ##### first fft (latitudinal direction)
  for (i in 1:NR) {
    xfft_n[i,] = fft(NOISE_MATRIX[i,])
  }
  ##   ' - - - - -  completed FT in x direction.'
  #####  second fft in longitudinal direction to generate the 2D DFT
  for (j in 1:NC) {
    fft2d_n[,j] = fft(xfft_n[,j])
  }
  ##   ' - - - - -  completed FT in y direction.'

  ##  convolution of isotropic power law filter with noise spectrum

  H_k_dftN = H_k * fft2d_n

  fft2d_i = matrix(NA,NR,NC) # inverse FT of structured noise 
  xfft_n  = matrix(NA,NR,NC) # temporary 2d array 
  for (i in 1:NR) {
      xfft_n[i,] = fft(H_k_dftN[i,], inverse = TRUE)  / NR
  }
  for (j in 1:NC) {
     fft2d_i[,j] = fft(xfft_n[,j], inverse = TRUE) / NR
  }
  Ns = Re(fft2d_i)
   ## standardise
  Ns = (Ns - mean(Ns)) / sd(Ns)
	return(Ns)
}


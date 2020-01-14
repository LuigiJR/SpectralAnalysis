
# CREATES A MAP OF SYNTHETIC NOISE WITH STRUCTURE FOLLOWING POWER LAW 

SyntheticStructuredNoise = function(NOISE,Beta,dx=2000) {
  # - NOISE  :  2D array of Gaussian white noise
	# dimensions NR, NC where
        # NR, NC  :  number of rows and columns over the areas of interets
  #              NOTE that these MUST be even numbers
  # - Beta :  power law exponent
  # - dx   :  spatial increment assumed to be 2000 m
  # Returns NR x NC 2D array of strctured noise

	NR =dim(NOISE)[1]; NC = NR

  # = = = =   Setting up for the Fourier analysis
  # - wavenumber ( pixel ** -1 ) 
  
	L  = NR * dx
  	xc = c(0:(NR/2),(NR/2-1):1)
	yc = xc
	gg = expand.grid(xc,yc)
	
	A = matrix(sqrt(gg$Var1*gg$Var1+gg$Var2*gg$Var2),NR,NC) 
	WL =  L / A  ## spatial wavelength m
	WN = 1./WL

# - Power law filter
	if (length(Beta) == 1) {
 		H_k = WN ** (-Beta/2.)
	} else {
		if (length(Beta) > 3) {
			print(' -- E R R O R: Currently only 3 Beta values permitted')
		} else {  ####  assumed breakpoints
		         w_beta3 = which(WL>=125000)	
			 w_beta2 = which(WL < 125000 & WL >= 17500)
			 w_beta1 = which(WL <  17500)

			 H_k = WN
			 H_k[w_beta3] = WN[w_beta3] ** (-Beta[3] / 2.)
			 H_k[w_beta2] = WN[w_beta2] ** (-Beta[2] / 2.)
			 H_k[w_beta1] = WN[w_beta1] ** (-Beta[1] / 2.)

			 H_k[w_beta3] = H_k[w_beta3] * (max(H_k[w_beta2]) / min(H_k[w_beta3]))
			 H_k[w_beta1] = H_k[w_beta1] * (min(H_k[w_beta2]) / max(H_k[w_beta1]))
		}
	}
  H_k[!is.finite(H_k)] = max(H_k[is.finite(H_k)]) 
  H_k[WL < 2*dx] = 0.e0  # Nyquist freq

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


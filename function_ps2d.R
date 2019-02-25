
# COMPUTES AND PLOTS THE FOURIER POWER SPECTRUM OF 2D ARRAY OF RAINFALL 

ps2d = function(X,dx,FTreturn=F) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal.
  # - dx : increment in both eastings and northings in meters (m)
  
#  - plotting colour ramp
  PowerRamp = colorRampPalette(c('navy','blue','cyan','green','yellow','orange','red','darkred'))
  NR = dim(X)[1]
  NC = dim(X)[2] 
  
  # = = = =   Setting up for the Fourier analysis
  # - wavelength increment (m) 
  delta_x = dx # m
  
  fft2d_rr = matrix(NA,NR,NC) # Rain data 2d Fourier transform
  xfft_rr  = matrix(NA,NR,NC) # temporary 2d array 

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
  ##### Power Spectrum
  PS_rr = Mod(fft2d_rr)**2
        # -- following centers the PS (i.e. low freq in middle
        PS_rr = PS_rr[c((NR/2):1,NR:(NR/2+1)),c((NC/2):1,NC:(NC/2+1))]
	
  PS_rst = raster(PS_rr,
		  crs='+proj=longlat +datum=WGS84',
		  xmn=-NC/(2.*(NC*delta_x)),xmx=NC/(2.*NC*delta_x),ymn=-NR/(2.*NR*delta_x),ymx=NR/(2.*NR*delta_x))
		# -  plot extent  +/- Nyquist critical frequency 

	PS_plot = log(PS_rst)
    dev.new();plot(PS_plot, col=PowerRamp(64))
}


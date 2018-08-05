
# COMPUTES AND PLOTS THE FOURIER POWER SPECTRUM OF 2D ARRAY OF RAINFALL 

ps2d = function(X,dx,Zmin=0,Zmax=100) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal.
  # - dx : increment in both eastings and northings in meters (m)
  # - Zmin / Zmax : minimum / maximum plottable value in 10*log10 units (default 100) 
  
#  - plotting colour ramp
  PowerRamp = colorRampPalette(c('white','navy','blue','cyan','green','yellow','orange','red','darkred'))
  NR = dim(X)[1]
  NC = NR
  
  # = = = =   Setting up for the Fourier analysis
  # - wavelength increment (m) 
  delta_x = dx # m
  
  # - wavenumber ( m ** -1 ) 
  wn = seq(0.0,by=1/(delta_x*NC),length.out=NC/2)
  
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

	
  PS_rst = raster(PS_rr[c((NR/2):1,1:(NR/2)),c((NR/2):1,1:(NR/2))],
		  crs='+proj=longlat +datum=WGS84',
		  xmn=-1/(2.*delta_x),xmx=1/(2.*delta_x),ymn=-1/(2.*delta_x),ymx=1/(2.*delta_x))
		# -  plot extent  +/- Nyquist critical frequency 

	PS_plot = 10*log10(PS_rst)
  	PS_plot[PS_plot > Zmax] = Zmax
  	PS_plot[PS_plot < Zmin] = Zmin
    dev.new();plot(PS_plot, col=PowerRamp(64),zlim=c(Zmin,Zmax))
}


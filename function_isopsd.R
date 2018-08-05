
# COMPUTES AND PLOTS THE ISOTROPIC POWER SPECTRAL DENSITY

isopsd = function(X,dx,Plotting=TRUE) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal.
  # - dx : increment in both eastings and northings in meters (m) 
  # - Plotting : to plot or not. if not, wavenumber and iso psd returned

  NR = dim(X)[1]
  NC = NR
  
  # = = = =   Setting up for the Fourier analysis
  # - wavelength increment (m)
  delta_x = dx # m
 
  # - wavenumber ( m ** -1 ) 
  wn = seq(0.0,by=1/(delta_x*NC),length.out=NC/2)
  
  # -  matrix of wavenumbers (needed for the radial averaging.
  A = matrix(NA,NC/2,NC/2)
  for (i in 1:(NC/2)) {for (j in 1:(NC/2)){A[i,j] = sqrt(wn[i]**2 + wn[j]**2)}}
  WAVENUMBERS = cbind(rbind(A,A[(NC/2):1,]),rbind(A[,(NC/2):1],A[(NC/2):1,(NC/2):1]))

  fft2d_rr = matrix(NA,NR,NC) # Rain data 2d Fourier transform
  xfft_rr  = matrix(NA,NR,NC) # temporary 2d array 

  ##### first fft (longitudinal direction)
  for (i in 1:NR) {
    xfft_rr[i,] = fft(X[i,])
  }
  ##print(  ' - - - - -  completed FT in x direction.')
  #####  second fft in latitudinal direction to generate the 2D DFT
  for (j in 1:NC) {
    fft2d_rr[,j] = fft(xfft_rr[,j])
  }
  ##print(  ' - - - - -  completed FT in y direction.')
  #
  ##### Power Spectrum
  PS_rr = Mod(fft2d_rr) **2

  # - isotropic (radially averaged power spectral density)
  ISO_PS = c()
  for (ip in 1:length(wn)) {
    bin.lim = wn[ip] + 1/(c(-2,2)*NC*delta_x)
    in.bin  = which(WAVENUMBERS >= bin.lim[1] & WAVENUMBERS < bin.lim[2])
    ISO_PS = c(ISO_PS,mean(PS_rr[in.bin],na.rm=T))
  }
  
  if (!Plotting) {
    return(list(Wavenummber = wn,PSD=ISO_PS))
  }  else {
	  # -  x-axis plot extent is 0 - Nyquist critical frequency (m ** -1)
    dev.new();plot(log10(wn), 10*log10(ISO_PS),type='l',col='blue',lwd=3,xlab='log10(k)')  
  } 
  
}


# COMPUTES AND PLOTS THE ISOTROPIC POWER SPECTRAL DENSITY

isopsd = function(X,dx,Plotting=FALSE) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal.
  # - dx : increment in both eastings and northings in meters (m) 
  # - Plotting : to plot or not. if not, only wavenumber and iso psd returned

  NR = dim(X)[1]
  NC = dim(X)[2] #NR
  
  # = = = =   Setting up for the Fourier analysis
  # - wavelength increment (m)
  delta_x = dx # m
 
  # - wavenumber ( m ** -1 ) 

  wn_x = seq((-NC/2+1)/(delta_x*NC),by=1/(delta_x*NC),length.out=NC)
  wn_y = seq((-NR/2+1)/(delta_x*NR),by=1/(delta_x*NR),length.out=NR)
	wn = abs(wn_x[(NC/2):1])  # -- for plotting x-axis of ISO PSD
  # -  matrix of wavenumbers (needed for the radial averaging.
  WAVENUMBERS = matrix(NA,NR,NC)
	for (j in 1:NC) {WAVENUMBERS[,j] = sqrt(wn_x[j]**2 + wn_y**2)}

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
        # -- following centers the PS (i.e. low freq in middle
	PS_rr = PS_rr[c((NR/2):1,NR:(NR/2+1)),c((NC/2):1,NC:(NC/2+1))]

  # - isotropic (radially averaged power spectral density)
  ISO_PS = c()
  for (ip in 1:length(wn)) {
	  bin.lim = wn[ip] + c(-1,1)*(1./(delta_x*NC))
          in.bin  = which(WAVENUMBERS >= bin.lim[1] & WAVENUMBERS < bin.lim[2])

	  ISO_PS = c(ISO_PS,mean(PS_rr[in.bin],na.rm=T))
  }
  
  ## spectral slope cal.
  BETA = c(NA,NA,NA)
  ALPHA = c(NA,NA,NA)
  wl = 1./wn
  # wl in small-scale 4 - 17.5 km
	iin = which(wl < 17500)
  	lmreg = lm(log10(ISO_PS[iin]) ~ log10(wl[iin]))
	 ALPHA[1] = as.numeric(coefficients(lmreg)[1])
	 BETA[1]  = as.numeric(coefficients(lmreg)[2])
# wl in meso-scale 17.5 - 125 km
 	iin = which(wl < 125000 & wl >= 17500)
	 lmreg = lm(log10(ISO_PS[iin]) ~ log10(wl[iin]))
	 ALPHA[2] = as.numeric(coefficients(lmreg)[1])
	 BETA[2]  = as.numeric(coefficients(lmreg)[2])
 # wl in large-scale 4 - 17.5 km
        iin = which(wl >= 125000 & is.finite(wl))
        lmreg = lm(log10(ISO_PS[iin]) ~ log10(wl[iin]))
         ALPHA[3] = as.numeric(coefficients(lmreg)[1])
         BETA[3]  = as.numeric(coefficients(lmreg)[2])

  if (Plotting) {
#	  # -  x-axis plot extent is 0 - Nyquist critical frequency (m ** -1)
    dev.new();plot(log10(wn), 10*log10(ISO_PS),type='l',col='blue',lwd=3,xlab='log10(k)')  
	if (!is.na(ALPHA[1]) & !is.na(BETA[1])) {
		lines(log10(wn),10*(ALPHA[1] - BETA[1]*log10(1./wl)),col='blue',lty=3)
	} 
          if (!is.na(ALPHA[2]) & !is.na(BETA[2])) {
                lines(log10(wn),10*(ALPHA[2] - BETA[2]*log10(1./wl)),col='blue',lty=3)
        }
        if (!is.na(ALPHA[3]) & !is.na(BETA[3])) {
                lines(log10(wn),10*(ALPHA[3] - BETA[3]*log10(1./wl)),col='blue',lty=3)
        }
  } 
  return(list(Wavenumber = wn,PSD=ISO_PS, Alpha = ALPHA,Beta = BETA))  
}


# COMPUTES AND PLOTS THE ISOTROPIC POWER SPECTRAL DENSITY

isopsd = function(X,dx=2000,Plotting=FALSE) {
  # - X : is the image 2D array of NR rows and NC columns.  For now
  #     NR = NC but they needn't be equal.
  # - dx : increment in both eastings and northings in meters (m, default 2km) 
  # - Plotting : to plot or not. if not, only wavenumber and iso psd returned

	IMAGE_IS_RASTER = is.object(X)
	if (IMAGE_IS_RASTER) {
		X = as.matrix(X)
	}
  NR = dim(X)[1]
  NC = dim(X)[2] #NR
  
  # = = = =   Setting up for the Fourier analysis
  # - wavelength increment (m)
  delta_x = dx # m
 
  # - wavenumber ( m ** -1 ) 

  wn_x = seq(-NC/2+1,by=1,length.out=NC)
  wn_y = seq(-NR/2+1,by=1,length.out=NR)
	wn = abs(wn_x[(NC/2):1])  # -- for plotting x-axis of ISO PSD
  # -  matrix of wavenumbers (needed for the radial averaging.
  WAVENUMBERS = matrix(NA,NR,NC)
	for (j in 1:NC) {WAVENUMBERS[,j] = sqrt(wn_x[j]**2 + wn_y**2)}
	WAVENUMBERS = round(WAVENUMBERS,0)

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
          in.bin  = which(WAVENUMBERS == wn[ip]) 
	  ISO_PS = c(ISO_PS,mean(PS_rr[in.bin],na.rm=T))
  }

  ## ISO PSD normalised to the center wavenumber:  falcialte comparison
	y_iso = ISO_PS/ISO_PS[floor(NC/2)] 
 
  ## spectral slope cal.
  BETA = c(NA,NA,NA); BETA_SD = c(NA,NA,NA)
  ALPHA = c(NA,NA,NA); ALPHA_SD = c(NA,NA,NA)
  wl = (NC*delta_x) / wn
  # wl in small-scale < 20 km
	iin = which(wl < 20000)
  	lmreg = lm(log10(y_iso[iin]) ~ log10(wl[iin]))
	 ALPHA[1] = as.numeric(coefficients(lmreg)[1])
	 BETA[1]  = as.numeric(coefficients(lmreg)[2])
	 BETA_SD[1] = summary(lmreg)$coefficients[2,2]
	 ALPHA_SD[1] = summary(lmreg)$coefficients[1,2]

# wl in meso-scale 20  - 130 km
 	iin = which(wl < 130000 & wl >= 20000)
	iin_meso = iin 
	 lmreg = lm(log10(y_iso[iin]) ~ log10(wl[iin]))
	 ALPHA[2] = as.numeric(coefficients(lmreg)[1])
	 BETA[2]  = as.numeric(coefficients(lmreg)[2])
         BETA_SD[2]  = summary(lmreg)$coefficients[2,2]
         ALPHA_SD[2] = summary(lmreg)$coefficients[1,2]
	
 # wl in large-scale > 130 km
        iin = which(wl >= 130000 & is.finite(wl))
        lmreg = lm(log10(y_iso[iin]) ~ log10(wl[iin]))
         ALPHA[3] = as.numeric(coefficients(lmreg)[1])
         BETA[3]  = as.numeric(coefficients(lmreg)[2])
         BETA_SD[3]  = summary(lmreg)$coefficients[2,2]
         ALPHA_SD[3] = summary(lmreg)$coefficients[1,2]

  if (Plotting) {
#	  # -  x-axis plot extent is 0 - Nyquist critical frequency (m ** -1)
	  # -  y-axis 10log10(ISOPSD) --> normalised to power at center wavenumber
    dev.new();plot(log10(1./wl), 10*log10(y_iso),type='l',col='blue',lwd=3,xlab='log10(k)',ylab='10*log10(ISO PSD)')  
	if (!is.na(ALPHA[1]) & !is.na(BETA[1])) {
		lines(log10(1./wl),10*(ALPHA[1] - BETA[1]*log10(1./wl)),col='blue',lty=3)
	} 
          if (!is.na(ALPHA[2]) & !is.na(BETA[2])) {
                lines(log10(1./wl),10*(ALPHA[2] - BETA[2]*log10(1./wl)),col='magenta',lty=3)
		lines(log10(1./wl[iin_meso]), 10*log10(y_iso[iin_meso]),col='magenta',lwd=3)
		points(log10(1./wl[iin_meso]),10*log10(y_iso[iin_meso]),col='magenta')
        }
        if (!is.na(ALPHA[3]) & !is.na(BETA[3])) {
                lines(log10(1./wl),10*(ALPHA[3] - BETA[3]*log10(1./wl)),col='blue',lty=3)
        }
  } 
  return(list(Wavenumber = 1./wl,PSD=y_iso, Alpha = ALPHA,Beta = BETA, Beta_StErr = BETA_SD))  
}

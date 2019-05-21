# PERFORM THE FOURIER DECOMPISTION OF A RAINFALL IMAGE TO OBTAIN THE
#   CASCDE LEVELS.  BASED ON USING GAUSSIAN WEIGHTS

cascadeDecomposition = function(R,dx,NumCascadeLevels=8) {
  # - R    : is the image 2D array of rainfall/logRainfall NR rows and NC columns.  
  #     For now  NR = NC but they needn't be equal.
  # - dx : pixel resolution (m or km)
  # - numCascadeLevels : number of casscade levels in the decomposition. Default = 8
  # 
  #  Returns:
  # - R.decomp : a list comprised of
  #           $Z     : a matrix with NR*NC (number of rows) by NumCascadeLevels cols.
  #                    containing the spatially standardised (i.e. N(0,1)) cascade
  #                    components.    
  #           $mu    : a vector of cascade level spatial means
  #           $sigma : a vector of cascade level spatial std devs. 
  #
  # Needs:
  #        source('./function_dft2d.R')
  #        source('./function_gaussianWeights.R')
  #
  
  filterButterworth = function(x,x_center,x_cutoff) {
    #  Butterworth filter  (from wikipedia)
    #   x : vector of spatial frequencies
    #   x_center :  center frequ. for bandpass filter  
    #   X_cutoff :  akin to 2*sigma, limit on the spatial 
    #               freq. in this case
    #   See wiki page for eps and n definitions.
    
    
    eps = 1.0
    n   = 6
    
    g = 1./sqrt(1. + eps**2 * ((x - x_center) / x_cutoff)**(2.*n))
    
    return(g)
  }
  
  gaussianWeights = function(wn,wn_c,dx,N,num_cascade_levels,cascade_level) {
    gs = function(x,mu,sigma)  {
      return(exp(-(x-mu)**2/(2.*sigma**2)))
    }
    
    L = N*dx # domain size in km
    
    # Nyquist freq
    nu_Nq  = 1./(2.*dx)
    
    # q   is the reciprocal of the scale reduction factor
    #    chosen such that the final cascade level K -1 has 
    #    a central frequency = nu_Nq (i.e. 1/2*dx)
    
    q = (L * nu_Nq)**(1/(num_cascade_levels-1))
    
    nu_k = log(wn); nu_c = log(wn_c); dnu_k = log(q)
    
    w = gs(nu_k,nu_c,0.333*dnu_k)
    
    if (cascade_level ==1) {
      w[wn < 1/L] = 1
    }
    
    
    return(w)  
  }
  
  ##  image dimensions
  NR = dim(R)[1]; NC = dim(R)[2]
  L = max(NR,NC) * dx
  isNA = which(is.na(R))

  #  wavenumbers 
  wn_x = seq((-NC/2+1)/(dx*NC),by=1/(dx*NC),length.out=NC)
  wn_y = seq((-NR/2+1)/(dx*NR),by=1/(dx*NR),length.out=NR)
  
  # -  matrix of wavenumbers (needed for the radial averaging.
  WAVENUMBERS = matrix(NA,NR,NC)
  for (j in 1:NC) {WAVENUMBERS[,j] = sqrt(wn_x[j]**2 + wn_y**2)}

  # - center freq, for cascade level
  q       = (L/(2*dx))**(1/(NumCascadeLevels-1))
  freq_c  = q**(0:(NumCascadeLevels-1))/L
  
  
  
  #
  R.decomp = list(Z     = matrix(NA,NR*NC,NumCascadeLevels),
                  mu    = matrix(NA,NumCascadeLevels,1),
                  sigma = matrix(NA,NumCascadeLevels,1),
                  q     = q, freq_c = freq_c)

    # dft 
    R[isNA] = 0.e0
    x.dft = dft2d(R,shift=TRUE)

    for (k in 1:NumCascadeLevels)    {

      weights = filterButterworth(log(WAVENUMBERS),log(freq_c[k]),0.5*log(q))
      weights = matrix(weights,NR,NC)
#      weights[WAVENUMBERS > 1./(2.*dx)] = 0.e0
    
    ##  filtered DFT
    
      x.dft.h = x.dft*weights
      x.dft.h = x.dft.h[c((NR/2):1,NR:(NR/2+1)),c((NC/2):1,NC:(NC/2+1))]
      x.idft = dft2d(x.dft.h,inverse=TRUE)
      x_k = Re(x.idft)
    
      mu_k    = mean(x_k)
      sigma_k =  sd(x_k)
    
      x_z = (x_k - mu_k)/sigma_k
   
      R.decomp$Z[,k]    = as.vector(x_z)
      R.decomp$mu[k]    = mu_k
      R.decomp$sigma[k] = sigma_k

  }
  return(R.decomp) 
}  

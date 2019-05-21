filterButterworth = function(x,x_center,x_cutoff) {
  #  Butterworth filter  (from wikipedia)
  #   x : vector of spatial frequencies
  #   x_center :  center frequ. for bandpass filter  
  #   X_cutoff :  akin to 2*sigma, limit on the spatial 
  #               freq. in this case
  #   See wiki page for eps and n definitions.
  
  
  eps = 1.0
  n   = 8
  
  g = 1./sqrt(1. + eps**2 * ((x - x_center) / x_cutoff)**(2.*n))
  
  return(g)
}

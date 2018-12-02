# COMPUTES LOG/EXP TRANSFORM OF RAINFALL/LOG RAINFALL

logRain = function(R_in,inverse=FALSE,minRain = 0.1) {
  # - R_in    : is the image 2D array of rainfall/logRainfall NR rows and NC columns.  
  #     For now  NR = NC but they needn't be equal.
  # - inverse : deafault FALSE meaning log of R is calculated; 
  #                   if TRUE the exp(logRain) is computed.
  # - minRain : the rain/no rain threshold, default = 0.1 mm hr**-1
  #
  
  NR = dim(R)[1]; NC = NR
  isNA = which(is.na(R_in))
  logR = R_in
  if(!inverse) {
    isRain = round(logR,2) > minRain
    isNoRain = !isRain 
    logR[which(isNoRain)] = 0  
    logR[which(isRain)]  = log(R_in[which(isRain)]) - log(minRain)
    logR[isNA] = NA 
   R_out = logR 
  } else {
    logMinRain = log(minRain)
    isRain = logR > 0.e0 
    isNoRain = !isRain
    R = exp(logR + logMinRain)
    R[which(isNoRain)] = 0.e0
    R[isNA] = NA
   R_out = R  
  }
  return(R_out) 
}  

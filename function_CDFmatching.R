#  APPLIES CUMMULATIVE DISTRIBUTION FUNCTION (CDF)
#    TRANSFORMATIONS TO MATCH DISTRIBTIONS FROM 
#    A SOURCE RAINFALL DATA SET TO THAT OF A TARGET 
#    RAINFALL DATA SET

CDFmatching = function(SOURCE,TARGET) {
  # -  SOURCE : 2D array of the rainfall data that is to be transformed
  # -  TARGET : 2D array of the reference rainfall to serve as the
  #        target CDF
  #  
  #   Note that the SOURCE and TARGET rainfall data must be of the
  #     same dimension and be matrices (not raster objects)
  #
  #  RETURNS - 2D array of CDF matched rainfall estimates
  #
  
    NR = nrow(SOURCE); NC = ncol(SOURCE)
    VALID = which(!is.na(TARGET))  # which of the target data are NAN
   
    x_src = sort(as.vector(SOURCE[VALID]))
    y_tar = sort(as.vector(TARGET[VALID]))
   
    y_src = approx(x=x_src,y=y_tar,xout = as.vector(SOURCE),rule=2)
   return(matrix(y_src$y,NR,NC))
}

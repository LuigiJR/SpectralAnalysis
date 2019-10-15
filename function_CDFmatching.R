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
    SOURCE_IS_RASTER = is.object(SOURCE)
    if (SOURCE_IS_RASTER) {
            rst_template = SOURCE
            SOURCE = as.matrix(SOURCE)
    }
    if (is.object(TARGET)) {TARGET  = as.matrix(TARGET) }

        if (is.matrix(SOURCE)) {
                NR = nrow(SOURCE); NC = ncol(SOURCE)
        }

        VALID = which(!is.na(TARGET))  # which of the target data are NAN

#    x_src = sort(as.vector(SOURCE[VALID]))
#    y_tar = sort(as.vector(TARGET[VALID]))

#       x_src = as.numeric(quantile(SOURCE[VALID],probs=c(0,0.02,0.1,0.25,0.5,0.75,0.9,0.98,1)))
        x_src = as.numeric(quantile(SOURCE[VALID],probs=seq(0,1,0.002)))
#       y_tar = as.numeric(quantile(TARGET[VALID],probs=c(0,0.02,0.1,0.25,0.5,0.75,0.9,0.98,1)))
        y_tar = as.numeric(quantile(TARGET[VALID],probs=seq(0,1,0.002)))

#    x_unique = unique(x_src)
#    y_unique = approx(x=x_src,y=y_tar,xout = x_unique,rule=2)$y

#    y_src = approx(x=x_unique,y=y_unique,xout = as.vector(SOURCE),rule=2)$y
        y_src = approx(x=x_src,y=y_tar,xout = as.vector(SOURCE),rule=2)$y

    ## to perform linear extrapolation for "xout" beyond x
    ##          Inspiration from https://rdrr.io/cran/Hmisc/src/R/Misc.s
    ##
#     x_src=x_unique
#     y_tar=y_unique
#     rng = range(x_src)
#     zz  = as.vector(SOURCE) > rng[2]
#     nz  = length(y_tar)
#
#    if (any(zz)) {y_src[zz] = (y_tar[nz] - y_tar[nz-1]) /
#                              (x_src[nz] - x_src[nz-1]) *
#                              (as.vector(SOURCE)[zz] - x_src[nz-1]) +
#                               y_tar[nz-1]}
   if (SOURCE_IS_RASTER) {
          OUTPUT = raster(matrix(y_src,NR,NC),template = rst_template)
   } else {
          if (is.matrix(SOURCE)) {
                OUTPUT = matrix(y_src,NR,NC)
          } else {
                OUTPUT = y_src
          }
   }
   return(OUTPUT)
}

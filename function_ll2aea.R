### transform latitude and longitude points in to Albers Conic (equal area(projection   
ll2aea = function( lat, lon ) {
  ## ----- converts lat long coordinate into
  #        Albers Equal-Area Conical Proj. E and N
  #
  #    TEST:  Lat/Long = -22.363088861402,133.775136
  #               N/E  = -2407534.020984, -22959.978483

  #                       -20.390625, 159.51784739848
  #                      -2451281.722321, 2631879.943991
  #
  #                       -46.40625, 151.875
  #                        -5163682.336386, 1448984.069194
  #
  #                       -31.640625, 106.171875
  #                       -3730791.739059,-2594343.143593
  ####  GDA94
  #RsemiMajor  = 6378137.0
  a = 6378137.0
  #InverseFlattening = 298.257222101
  f = 1./ 298.257232666016 #1./298.257222101
  e = sqrt( 2. * f -  f * f )

  #lon_0  = 134.0  # reference logitude
  lon_0  = 132.0  # reference logitude
  lat_0  = 0.000  # reference latitude
  lat_1  = -18.00 # two standard parallels
  lat_2  = -36.00

  d2r = pi / 1.8e+2


  m = cos(d2r*lat) / sqrt(1. - e**2 * sin(d2r*lat)**2)
  m1 = cos(d2r*lat_1) / sqrt(1. - e**2 * sin(d2r*lat_1)**2)
  m2 = cos(d2r*lat_2) / sqrt(1. - e**2 * sin(d2r*lat_2)**2)

  q  = (1 - e**2)*(sin(d2r*lat)   / (1 - e**2 * sin(d2r*lat)**2)   - (1./(2.*e))*log((1 - e*sin(d2r*lat))/(1   + e*sin(d2r*lat))))
  q0 = (1 - e**2)*(sin(d2r*lat_0) / (1 - e**2 * sin(d2r*lat_0)**2) - (1./(2.*e))*log((1 - e*sin(d2r*lat_0))/(1 + e*sin(d2r*lat_0))))
  q1 = (1 - e**2)*(sin(d2r*lat_1) / (1 - e**2 * sin(d2r*lat_1)**2) - (1./(2.*e))*log((1 - e*sin(d2r*lat_1))/(1 + e*sin(d2r*lat_1))))
  q2 = (1 - e**2)*(sin(d2r*lat_2) / (1 - e**2 * sin(d2r*lat_2)**2) - (1./(2.*e))*log((1 - e*sin(d2r*lat_2))/(1 + e*sin(d2r*lat_2))))

  n = (m1**2 - m2**2) / (q2 - q1)
  theta = n * ( lon - lon_0 )
  C = m1**2 + n*q1

  rho   = a * sqrt(C - n*q)  / n
  rho_0 = a * sqrt(C - n*q0) / n

  k = sqrt(C - n*q) / m
  h = 1./k

  x = rho*sin(d2r*theta)
  y = rho_0 - rho*cos(d2r*theta)

  return(list(E = x,
              N = y))
}

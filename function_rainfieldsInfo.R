rainfieldsInfo = function()
{
  proj  = '+proj=aea +lat_1=-18.0 +lat_2=-36.0 +lon_0=132 +lat_0=0 +datum=WGS84'
  nrows = 2050; ncols = 2450
  Eastings = seq(-2299000,2599000,2000); Northings = seq(-5099000,-1001000,2000)
  Xmn = -2300000; Xmx = 2600000; Ymn = -5100000; Ymx = -1000000
  return(list(proj=proj,nrows=nrows,ncols=ncols,
              Eastings=Eastings,Northings=Northings,
              Xmn=Xmn,Xmx=Xmx,Ymn=Ymn,Ymx=Ymx))
}


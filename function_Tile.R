#  Specifcy the Tile Latitude and Longitude for the SEAMLESS TILE mosaic
#
Tile = function(TILE_NAME) {

	SEAMLESS_Tile_Lons = c(145.0,150.874,142.0,134,125,117,117,125,134,142.0,150,125.5,134,140,147,117.0)
	SEAMLESS_Tile_Lats = c(-39.5,-33.264,-32.0,-31,-31,-31,-23,-23,-23,-23.5,-25,-15.0,-15,-15,-17,-15.0)
	SEAMLESS_Tile_Name = c('VICTAS','SYDM','W_NSW','SA','SE_WA','SW_WA','NW_WA','NE_WA','N_SA','W_QLD','SE_QLD','NW_NT','NT','N_QLD','NE_QLD','NW_WA_COAST')
	SEAMLESS_Tile_Name_short = c('VICTAS','SYDM','W_NSW','SA','SE_WA','SW_WA','NW_WA','NE_WA','N_SA','W_QLD','SE_QLD','NW_NT','NT','N_QLD','NE_QLD','NW_WA_COAST')

	k_id = which(SEAMLESS_Tile_Name == TILE_NAME | SEAMLESS_Tile_Name_short == TILE_NAME) 
 return(c(SEAMLESS_Tile_Lats[k_id],SEAMLESS_Tile_Lons[k_id]))
}


nl_clumpdetect <- function(raster_obj, shp){
  
  nl_data <- raster_obj
  
  nl_clump <- raster::clump(nl_data)
  
  nl_clump[nl_clump > 0] <- 1
  
  nl_clump[is.na(nl_clump)] <- 0
  
  nl_clump <- mask(nl_clump, shp)
  
  nl_clump
  
}
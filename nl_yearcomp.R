nl_yearcomp <- function(month, year, shp, pais, mineria){
  
  rasterOptions(maxmemory = 1e+09)
  
  if(!file.exists(sprintf("raster/%s.tif", paste(year, month, sep="")))){
  
    nl_stacks <- nl_rastercomp(month, year, shp, pais, mineria)
    
    writeRaster(nl_stacks, filename = sprintf("raster/%s.tif", paste(year, month, sep="")))
    
  } else {
    
    nl_stacks <- raster(sprintf("raster/%s.tif", paste(year, month, sep="")))
    
  }
  
  nl_stacks <- raster::extract(nl_stacks, shp, df=TRUE)
  
  colnames(nl_stacks) <- c("ID", "layer")
  
  nl_stacks <- nl_stacks %>%
    group_by(ID) %>%
    mutate(total_nl=sum(layer)) %>%
    select(ID, total_nl)
  
  nl_stacks <- unique(nl_stacks)
  
  if(pais==1 & mineria==0){
  
  nl_total <- data.frame(edo=shp$ADM1_ES, edo_id=shp$ADM1_PCODE,
                         mpo=shp$ADM2_ES, mpo_id=shp$ADM2_PCODE,
                         pqa=shp$ADM3_ES, pqa_id=shp$ADM3_PCODE,
                         nl_stacks$total_nl)
  }
  
  if(mineria==1 & pais==0){
    
    dataset <- shp
    
    st_geometry(dataset) <- NULL
    
    dataset <- as.data.frame(dataset)
    
    nl_total <- cbind(shp, nl_stacks$total_nl)
    
  }
  
  cat("Paso 6")
  
  nl_total
  
}
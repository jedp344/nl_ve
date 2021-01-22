nl_rastercomp <- function(month, year, shp, pais, mineria){
  
  
  library(sf)
  library(raster)
  library(spData)
  library(spDataLarge)
  library(dplyr)
  library(doParallel)
  library(foreach)
  library(parallel)
  
  shp <- shp
  shp_sample <- st_read("nolights_sample.shp")
  
  years_y <- data.frame(m=rep(1:12, 9), y=rep(2012:2020, each=12))
  
  years_y <- years_y[years_y$y<year | (years_y$y==year & years_y$m<=month),]
  
  years_y <- years_y[(nrow(years_y)-11):nrow(years_y),]
  
  years_y$m[nchar(years_y$m)==1] <- paste0(0, years_y$m[nchar(years_y$m)==1])
  
  cat(paste(years_y$m[1], years_y$y[1], years_y$m[12], years_y$y[12], sep="-"))
  
  cat("Paso 1")
  
  source('~/Fundacion/Nightlights Data/nldataproc.R')
  
  source('~/Fundacion/Nightlights Data/nl_clumpdet.R')
  
  UseCores <- detectCores()-1
  
  cl <- makeCluster(UseCores)
  
  registerDoParallel(cl)
  
  nl_stacks <- foreach(i=1:nrow(years_y), .combine = 'stack', .export = c("nl_bgnoise", "nl_clumpdetect"), .packages = c("raster", "sf")) %dopar% {
    
    if(!file.exists(paste0("tmp/", years_y$y[i], years_y$m[i], ".tif"))){
    
      nl_data <- nl_bgnoise(years_y$m[i], years_y$y[i], shp, shp_sample, pais, mineria)
      
      writeRaster(nl_data, paste0("tmp/", years_y$y[i], years_y$m[i], ".tif"))
      
      nl_datasets <- nl_data
    
    } else {
      
      nl_data <- raster(paste0("tmp/", years_y$y[i], years_y$m[i], ".tif"))
      
      nl_datasets <- nl_data
      
    }
    
    out <- nl_datasets
    
    return(out)
    
  }
  
  cat("Paso 2")
  
  nl_clumps <- foreach(i=1:nrow(years_y), .combine = 'stack', .export = c("nl_bgnoise", "nl_clumpdetect"), .packages = c("raster", "sf")) %dopar% {
   
    nl_data <- raster(paste0("tmp/", years_y$y[i], years_y$m[i], ".tif"))
    
    nl_clump <- nl_clumpdetect(nl_data, shp)
    
    nl_clumps <- nl_clump 
    
    return(nl_clumps)
    
  }

  stopCluster(cl)
  
  cat("Paso 3")
    
  beginCluster(3)
  
  nl_clumps <- clusterR(nl_clumps, calc, args = list(sum))
  
  endCluster()

  beginCluster(3)
  
  nl_stacks <- clusterR(nl_stacks, calc, args = list(mean, na.rm=T))
  
  endCluster()
  
  nl_recurrent <- nl_clumps
  
  cat("Paso 4")
  
  nl_stacks[nl_recurrent<4] <- 0
  
  nl_stacks[nl_stacks<=0.4] <- 0
  
  nl_stacks <- mask(nl_stacks, shp)
  
  nl_recurrent <- mask(nl_stacks, shp)
  
  cat("Paso 5")
  
  nl_stacks <- nl_firedet(nl_stacks)
  
  gc()
  
  nl_stacks
  
}
nl_bgnoise <- function(month, year, shp, shp_sample, pais, mineria){

  library(sf)
  library(raster)
  library(spData)
  library(spDataLarge)
  library(dplyr)
  
  nl_data <- raster(paste0("data/", year, month,"/", year, month, ".tif"))
  nl_data <- crop(nl_data, shp)
  nl_data <- mask(nl_data, shp)
  
  nolights <- crop(nl_data, shp_sample)
  nolights <- mask(nolights, shp_sample)
  nolights[nolights >= 0.5] <- 0
  
  nolights_val <- raster::extract(nolights, shp_sample, df=TRUE)
  
  dataname <- paste0("nolights_val$X", year, month,"[nolights_val$ID==")
  
  nolights1  <- max(eval(parse(text=paste0(dataname, 1, "]"))))
  nolights2  <- max(eval(parse(text=paste0(dataname, 2, "]"))))
  nolights3  <- max(eval(parse(text=paste0(dataname, 3, "]"))))
  nolights4  <- max(eval(parse(text=paste0(dataname, 4, "]"))))
  
  
  cuts <- c(0, 0.7, 1, 2, 3, 5, 10, 20, 30, 40, 80, 100, 200, 500, 1000, 3000, 5000, 10000, 30000)
  
  if(pais==1 & mineria==0){
  
  shp2 <- shp %>% filter(COD_ESTADO==13 | COD_ESTADO==23 | COD_ESTADO==6 | COD_ESTADO==20|
                           COD_ESTADO==21| COD_ESTADO==11 | COD_ESTADO==14 | COD_ESTADO==18 |
                           COD_ESTADO==4)
  
  nl1 <- crop(nl_data, shp2)
  nl1 <- mask(nl1, shp2)
  nl1[nl1 <= nolights1] <- 0
  
  shp3 <- shp %>% filter(COD_ESTADO==16 | COD_ESTADO==15 | COD_ESTADO==12 | COD_ESTADO==9|
                           COD_ESTADO==8| COD_ESTADO==5 | COD_ESTADO==3 | COD_ESTADO==1 |
                           COD_ESTADO==17 | COD_ESTADO==19 | COD_ESTADO==24 | COD_ESTADO==22)
  
  nl2 <- crop(nl_data, shp3)
  nl2 <- mask(nl2, shp3)
  nl2[nl2 <= nolights2] <- 0
  
  shp4 <- shp %>% filter(ADM2_PCODE=="VE0202" | ADM2_PCODE=="VE0203" | ADM2_PCODE=="VE0204" |
                           ADM2_PCODE=="VE0206" | COD_ESTADO==7 | COD_ESTADO==10)
  
  nl3 <- crop(nl_data, shp4)
  nl3 <- mask(nl3, shp4)
  nl3[nl3 <= nolights3] <- 0
  
  shp5 <- shp %>% filter(ADM2_PCODE=="VE0201" | ADM2_PCODE=="VE0205" | ADM2_PCODE=="VE0207")
  
  nl4 <- crop(nl_data, shp5)
  nl4 <- mask(nl4, shp5)
  nl4[nl4 <= nolights4] <- 0
  
  nl_data <- raster::merge(nl1, nl2)
  nl_data <- raster::merge(nl_data, nl3)
  nl_data <- raster::merge(nl_data, nl4)
  }
  
  if(mineria==1 & pais==0){
    
    nl3 <- crop(nl_data, shp)
    nl3 <- mask(nl3, shp)
    nl3[nl3 <= nolights3] <- 0
    
    nl_data <- nl3
    
  }
  
  nl_data

}
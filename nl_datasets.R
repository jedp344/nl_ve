nl_dataset <- function(month_1, year_1, month_2, year_2, shp, pais, mineria){
  
  library(sf)
  library(raster)
  library(spData)
  library(spDataLarge)
  library(dplyr)
  
  years_d <- data.frame(m=rep(1:12, length(year_1:year_2)), y=rep(year_1:year_2, each=12))
  
  years_d <- years_d[years_d$y>year_1 | (years_d$y==year_1 & years_d$m>=month_1),]
  
  years_d <- years_d[years_d$y<year_2 | (years_d$y==year_2 & years_d$m<=month_2),]
  
  for(i in 1:nrow(years_d)){
    
    nl_data <- nl_yearcomp(years_d$m[i], years_d$y[i], shp, pais, mineria)
    
    if(!exists("nld")){
      
      cat(format(as.Date(sprintf("%s-%s-1", years_d$y[i], years_d$m[i]), format="%Y-%m-%d"), format="%B-%y"))
      
      nld <- nl_data
      
      colnames(nld)[length(nld)] <- format(as.Date(sprintf("%s-%s-1", years_d$y[i], years_d$m[i]), format="%Y-%m-%d"), format="%B-%y")
      
    } else {
      
      if(pais==1){
      
      nld <- merge(nld, nl_data, by=c("edo", "edo_id", "mpo", "mpo_id", "pqa", "pqa_id"))
      
      colnames(nld)[length(nld)-1] <- format(as.Date(sprintf("%s-%s-1", years_d$y[i], years_d$m[i]), format="%Y-%m-%d"), format="%B-%y")
      
      } else {
        
        nl_data <- nl_data[(length(nl_data)-1)]
        
        st_geometry(nl_data) <- NULL
        
        nld <- cbind(nld, nl_data)
        
        colnames(nld)[length(nld)-1] <- format(as.Date(sprintf("%s-%s-1", years_d$y[i], years_d$m[i]), format="%Y-%m-%d"), format="%B-%y")
        
      }
    }
    
  }
  
  nld
  
}
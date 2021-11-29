library(sf)
library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
all_df<-readRDS("../../Urban/Tables/US_Year_with_native.rda")

all_df$NATIVE2<-ifelse(all_df$NATIVE=="NON-NATIVE", "NON-NATIVE", "NATIVE_WITH_BUFFER")
mask<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")
if (F){
  local_cols<-c("LOCALITY_ID", "LATITUDE", "LONGITUDE", "X", "Y")
  localities<-unique(all_df[, ..local_cols])
  saveRDS(localities, "../../Urban/Tables/US_localities.rda")
}
if (F){
  localities<-readRDS("../../Urban/Tables/US_localities.rda")
  ll_cols<-c("LONGITUDE", "LATITUDE")
  localities_points<-st_as_sf(localities, coords = ll_cols, crs=st_crs(mask_ll))
  localities_points<-st_transform(localities_points, crs=st_crs(mask))
  saveRDS(localities_points, "../../Urban/Shape/localities_points.rda")
}
localities_points<-readRDS("../../Urban/Shape/localities_points.rda")
cities<-st_read("../../Urban/Shape/tl_2017_us_uac10/tl_2017_us_uac10.shp")
cities<-st_transform(cities, crs=st_crs(mask))

city_buffer<-readRDS("../../Urban/Shape/City_buffer/selected_buffer_info.rda")
cities_list<-unique(city_buffer$city)
#city_str<-cities_list[grepl("Chicago, ", cities_list)]
#city_item<-city_buffer[city==city_str]
city_i=1


all_df<-all_df[YEAR>=2010]


city_buffer<-city_buffer[sample(nrow(city_buffer), nrow(city_buffer)),]
for (city_i in c(1:nrow(city_buffer))){
  city_item<-city_buffer[city_i,]
  city_str<-city_item$city
  city_name<-gsub("/", "-", (gsub(", ", "-", city_str)))
  
  print(paste(city_i, nrow(city_buffer), city_str))
  target<-sprintf("../../Urban/Tables/City_buffer_Occurrences/%s.rda", city_name)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  city_shape<-cities[which(cities$NAME10==city_str),]
  
  buffer_dist<-city_buffer[city==city_str]
  buffer<-readRDS(sprintf("../../Urban/Shape/City_buffer/Buffers/%s.rda", city_name))
  buffer_shape<-buffer[[as.character(buffer_dist$buffer_dist)]]
  
  
  index<-st_contains(buffer_shape, localities_points)
  indics<-c()
  for (i in c(1:length(index))){
    indics<-c(indics, index[[i]])
  }
  
  #buffer_localities<-localities[indics, ]
  buffer_points<-localities_points[indics, ]
  
  index<-st_contains(city_shape, localities_points)
  indics<-c()
  for (i in c(1:length(index))){
    indics<-c(indics, index[[i]])
  }
  city_points<-localities_points[indics,]
  
  df_buffer<-all_df[LOCALITY_ID %in% buffer_points$LOCALITY_ID]
  
  df_buffer$TYPE<-"BUFFER"
  
  df_city<-all_df[LOCALITY_ID %in% city_points$LOCALITY_ID]
  df_city$TYPE<-"CITY"
  df_city[LOCALITY_ID %in% df_buffer$LOCALITY_ID]
  df_alll<-rbindlist(list(df_city, df_buffer))
  
  saveRDS(df_alll, target)
  if (F){
    for (name in rev(names(buffer))){
      if (name=="10000"){
        plot(st_geometry(buffer[[name]]), main=paste(city_name, "buffer:", city_item$buffer_dist/1000, "km"))
      }else{
        if (name=="no"){
          plot(st_geometry(buffer[[name]]), add=T, col="grey")
        }else{
          plot(st_geometry(buffer[[name]]), add=T)
        }
      }
      
    }
    
    points(df_alll[TYPE=="BUFFER"]$X, df_alll[TYPE=="BUFFER"]$Y, pch=".", col="blue")
    points(df_alll[TYPE=="CITY"]$X, df_alll[TYPE=="CITY"]$Y, pch=".", col="red")
  }
}

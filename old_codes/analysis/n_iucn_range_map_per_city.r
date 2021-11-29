library(sf)
library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
#all_df<-readRDS("../../Urban/Tables/US_Year_with_native.rda")

mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")


sp_shape_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_shape.rda")
sp_buffer_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_buffer.rda")
cities<-st_read("../../Urban/Shape/tl_2017_us_uac10/tl_2017_us_uac10.shp")
city_buffer<-readRDS("../../Urban/Shape/City_buffer/selected_buffer_info.rda")
cities_list<-unique(city_buffer$city)
#city_str<-cities_list[grepl("Atlanta, GA", cities_list)]
#city_item<-city_buffer[city==city_str]
city_i=1

names(sp_buffer_list[[1]])

city_buffer<-city_buffer[sample(nrow(city_buffer), nrow(city_buffer)),]
for (city_i in c(1:nrow(city_buffer))){
  city_item<-city_buffer[city_i,]
  city_str<-city_item$city
  city_name<-gsub("/", "-", (gsub(", ", "-", city_str)))
  
  print(paste(city_i, nrow(city_buffer), city_str))
  target<-sprintf("../../Urban/Tables/City_buffer_IUCN/%s.rda", city_name)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  city_shape<-cities[which(cities$NAME10==city_str),]
  city_shape<-st_transform(city_shape, crs=st_crs(mask_10km))
  
  buffer_dist<-city_buffer[city==city_str]
  buffer<-readRDS(sprintf("../../Urban/Shape/City_buffer/Buffers/%s.rda", city_name))
  buffer_shape<-buffer[[as.character(buffer_dist$buffer_dist)]]
  buffer_shape<-st_transform(buffer_shape, crs=st_crs(mask_10km))
  if (F){
    plot(st_geometry(buffer_shape), col="blue")
    plot(st_geometry(city_shape), add=T, col="red")
  }
  sp<-names(sp_shape_list)[1]
  
  sp_i<-1
  sp_count<-data.table(Sp_Buffer=names(sp_buffer), N_City=0, N_City_Buffer=0)
  for (sp_i in c(1:length(names(sp_shape_list)))){
    sp<-names(sp_shape_list)[sp_i]
    print(paste(city_str, sp_i, length(sp_shape_list)))
    sp_shape<-sp_shape_list[[sp]]
    sp_buffer<-sp_buffer_list[[sp]]
    #sp_shape<-st_transform(sp_shape, crs=st_crs(mask_10km))
    #sp_buffer<-st_transform(sp_buffer, crs=st_crs(mask_10km))
    
    sp_buffer[["0"]]<-sp_shape
    df_item<-list()
    buf<-"0.5"
    
    for (buf in names(sp_buffer)){
      sp_shape_item<-sp_buffer[[buf]]
      sp_count[Sp_Buffer==buf]$N_City<-ifelse(length(unlist(st_intersects(sp_shape_item, city_shape)))>0, 
                                              sp_count[Sp_Buffer==buf]$N_City+1, sp_count[Sp_Buffer==buf]$N_City)  
      sp_count[Sp_Buffer==buf]$N_City_Buffer<-ifelse(length(unlist(st_intersects(sp_shape_item, buffer_shape)))>0, 
                                                     sp_count[Sp_Buffer==buf]$N_City_Buffer+1, sp_count[Sp_Buffer==buf]$N_City_Buffer)
    }
  }
  center<-st_coordinates(st_centroid(city_shape))
  if (F){
    
    plot(st_geometry(buffer_shape))
    plot(st_geometry(city_shape), add=T)
    points(center[1], center[2], col="red")
    
  }
  dfff<-data.frame(city=city_str, 
                   ratio_times=city_item$ratio_times,
                   city_area=city_item$city_area,
                   buffer_area=city_item$buffer_area,
                   buffer_dist=city_item$buffer_dist,
                   ratio=city_item$ratio,
                   centroid_x=center[1],
                   centroid_y=center[2],
                   city_sp_count=city_sp_count,
                   city_sp_buffer_count=city_sp_buffer_count,
                   city_buffer_sp_count=city_buffer_sp_count,
                   city_buffer_sp_buffer_count=city_buffer_sp_buffer_count)
  saveRDS(dfff, target)
}


city_buffer<-city_buffer[sample(nrow(city_buffer), nrow(city_buffer)),]
all_item<-list()
for (city_i in c(1:nrow(city_buffer))){
  city_item<-city_buffer[city_i,]
  city_str<-city_item$city
  city_name<-gsub("/", "-", (gsub(", ", "-", city_str)))
  
  print(paste(city_i, nrow(city_buffer), city_str))
  target<-sprintf("../../Urban/Tables/City_buffer_IUCN/%s.rda", city_name)
  item<-readRDS(target)
  all_item[[city_name]]<-item
}
all_item<-rbindlist(all_item)
saveRDS(all_item, "../../Urban/Tables/US_city_iucn_range_map_result.rda")
all_item<-readRDS("../../Urban/Tables/US_city_iucn_range_map_result.rda")
all_item[grepl("Chicago, ", city)]
all_city_result[grepl("Chicago, ", CITY)]

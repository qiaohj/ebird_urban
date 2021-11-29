library(sf)
library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
all_df<-readRDS("../../Urban/Tables/US_ALL_After_2015_with_native.rda")
unique(all_df$NATIVE)
unique(all_df$SEASONAL)

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


city_i=1
all_city_result<-list()
for (city_i in c(1:nrow(city_buffer))){
  city_item<-city_buffer[city_i,]
  city_str<-city_item$city
  city_name<-gsub("/", "-", (gsub(", ", "-", city_str)))
  
  print(paste(city_i, nrow(city_buffer), city_str))
  target<-sprintf("../../Urban/Tables/City_buffer_Occurrences/%s.rda", city_name)
  df_buffer<-readRDS(target)
  
  df_buffer_se<-df_buffer[, .(N_LOCALITY=length(unique(LOCALITY_ID)),
                              N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              N_EVENT=sum(EVENT_COUNT)),
                          by=list(NATIVE, YEAR)]
  
  df_buffer_se$TYPE<-"ALL-NATIVE-1"
  df_buffer_se$CITY<-city_item$city
  df_buffer_se$CITY_AREA<-city_item$city_area
  df_buffer_se$BUFFER_AREA<-city_item$buffer_area
  df_buffer_se$BUFFER_DIST<-city_item$buffer_dist
  df_buffer_se$NATIVE2<-NA
  df_city_se<-df_buffer[TYPE=="CITY", .(N_LOCALITY=length(unique(LOCALITY_ID)),
                              N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              N_EVENT=sum(EVENT_COUNT)),
                          by=list(NATIVE, YEAR)]
  
  df_city_se$TYPE<-"CITY-NATIVE-1"
  df_city_se$CITY<-city_item$city
  df_city_se$CITY_AREA<-city_item$city_area
  df_city_se$BUFFER_AREA<-city_item$buffer_area
  df_city_se$BUFFER_DIST<-city_item$buffer_dist
  df_city_se$NATIVE2<-NA
  
  df_buffer_only_se<-df_buffer[TYPE=="BUFFER", .(N_LOCALITY=length(unique(LOCALITY_ID)),
                                        N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                                        N_OBSERVATION=sum(OBSERVATION_COUNT),
                                        N_EVENT=sum(EVENT_COUNT)),
                        by=list(NATIVE, YEAR)]
  
  df_buffer_only_se$TYPE<-"BUFFER-NATIVE-1"
  df_buffer_only_se$CITY<-city_item$city
  df_buffer_only_se$CITY_AREA<-city_item$city_area
  df_buffer_only_se$BUFFER_AREA<-city_item$buffer_area
  df_buffer_only_se$BUFFER_DIST<-city_item$buffer_dist
  df_buffer_only_se$NATIVE2<-NA
  
  df_buffer_se2<-df_buffer[, .(N_LOCALITY=length(unique(LOCALITY_ID)),
                              N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              N_EVENT=sum(EVENT_COUNT)),
                          by=list(NATIVE2, YEAR)]
  
  df_buffer_se2$TYPE<-"ALL-NATIVE-2"
  df_buffer_se2$CITY<-city_item$city
  df_buffer_se2$CITY_AREA<-city_item$city_area
  df_buffer_se2$BUFFER_AREA<-city_item$buffer_area
  df_buffer_se2$BUFFER_DIST<-city_item$buffer_dist
  df_buffer_se2$NATIVE<-NA
  
  df_city_se2<-df_buffer[TYPE=="CITY", .(N_LOCALITY=length(unique(LOCALITY_ID)),
                                        N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                                        N_OBSERVATION=sum(OBSERVATION_COUNT),
                                        N_EVENT=sum(EVENT_COUNT)),
                        by=list(NATIVE2, YEAR)]
  
  df_city_se2$TYPE<-"CITY-NATIVE-2"
  df_city_se2$CITY<-city_item$city
  df_city_se2$CITY_AREA<-city_item$city_area
  df_city_se2$BUFFER_AREA<-city_item$buffer_area
  df_city_se2$BUFFER_DIST<-city_item$buffer_dist
  df_city_se2$NATIVE<-NA
  
  df_buffer_only_se2<-df_buffer[TYPE=="BUFFER", .(N_LOCALITY=length(unique(LOCALITY_ID)),
                                                 N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                                                 N_OBSERVATION=sum(OBSERVATION_COUNT),
                                                 N_EVENT=sum(EVENT_COUNT)),
                               by=list(NATIVE2, YEAR)]
  
  df_buffer_only_se2$TYPE<-"BUFFER-NATIVE-2"
  df_buffer_only_se2$CITY<-city_item$city
  df_buffer_only_se2$CITY_AREA<-city_item$city_area
  df_buffer_only_se2$BUFFER_AREA<-city_item$buffer_area
  df_buffer_only_se2$BUFFER_DIST<-city_item$buffer_dist
  df_buffer_only_se2$NATIVE<-NA
  
  df_item<-rbindlist(list(df_city_se, df_buffer_se, df_buffer_only_se,
                          df_city_se2, df_buffer_se2, df_buffer_only_se2),
                     use.names=TRUE)
  all_city_result[[city_str]]<-df_item
}
all_city_result<-rbindlist(all_city_result)
saveRDS(all_city_result, "../../Urban/Tables/US_city_result.rda")
all_city_result$CENTROID_X<--9999
all_city_result$CENTROID_Y<--9999


for (city_i in c(1:nrow(city_buffer))){
  
  city_item<-city_buffer[city_i,]
  city_str<-city_item$city
  city_name<-gsub("/", "-", (gsub(", ", "-", city_str)))
  print(paste(city_i, nrow(city_buffer), city_str))
  buffer<-readRDS(sprintf("../../Urban/Shape/City_buffer/Buffers/%s.rda", city_name))
  city<-buffer[["no"]]
  center<-st_centroid(city)
  
  all_city_result[CITY==city_str]$CENTROID_X<-st_coordinates(center)[1]
  all_city_result[CITY==city_str]$CENTROID_Y<-st_coordinates(center)[2]
  if (F){
    plot(st_geometry(city))
    plot(st_geometry(center), add=T, col="red")
  }
}
saveRDS(all_city_result, "../../Urban/Tables/US_city_result.rda")

library(sf)
library(raster)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
mask<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")
cities<-st_read("../../Urban/Shape/tl_2017_us_uac10/tl_2017_us_uac10.shp")
cities<-st_transform(cities, crs=st_crs(mask))
cities_list<-unique(cities$NAME10)
city<-cities_list[grepl("Atlanta, GA", cities_list)]
if (F){
  city_buffer<-readRDS(target)
  plot(st_geometry(city_buffer[["7000"]]))
  plot(st_geometry(cities), add=T)
  plot(st_geometry(city_buffer[["no"]]), add=T, col="red")
  xxx<-st_transform(city_buffer[["7000"]], crs=st_crs(mask_ll))
  st_write(xxx, "../../Urban/Shape/Atlanta_buffer_7000.shp")
}
all_buffer<-NULL

xx<-data.frame(i=c(1:length(cities_list)), city=cities_list)
xx[which(xx$city=="Bridgeport--Stamford, CT--NY"),]

for (city in cities_list){
  print(city)
  
  city_name<-gsub("/", "-", (gsub(", ", "-", city)))
  target<-sprintf("../../Urban/Shape/City_buffer/Buffers/%s.rda", city_name)
  #if (file.exists(target)){
  #  next()
  #}
  #saveRDS(NULL, target)
  if (T){
    if (file.exists(target)){
      feather<-cities[which(cities$NAME10==city),]
      city_buffer<-readRDS(target)
      city_buffer[["no"]]<-feather
      saveRDS(city_buffer, target)
      area_no<-as.numeric(st_area(city_buffer[["no"]]))
      for (dist in c(seq(10, 90, by=10), seq(100, 1000, by=100), seq(2000, 10000, by=1000))){
        buf_outer<-city_buffer[[as.character(dist)]]
        item<-data.frame(city_area=area_no, buffer_area=as.numeric(st_area(buf_outer)),
                         city=city, buffer_dist=dist)
        if (is.null(all_buffer)){
          all_buffer<-item
        }else{
          all_buffer<-rbind(all_buffer, item)
        }
      }
      
      next()
    }
  }
  feather<-cities[which(cities$NAME10==city),]
  #saveRDS(NULL, target)
  #city_buffer<-list()
  #city_buffer[["no"]]<-feather
  
  city_buffer<-readRDS(target)
  area_no<-as.numeric(st_area(feather))
  dist<-10
  for (dist in c(seq(10, 90, by=10), seq(100, 1000, by=100), seq(2000, 10000, by=1000))){
    print(paste(city, dist))
    buffer<-st_buffer(feather, dist)
    buf_inter<-st_intersection(buffer, cities)
    colnames(buf_inter)[1:24]<-paste("c", c(1:24), sep="")
    buf_inter<-st_union(buf_inter)
    buf_outer<-st_difference(buffer, buf_inter)
    
    #buf_outer<-st_union(buf_outer)
    if (F){
      buf_inter_ll<-st_transform(buf_inter, crs=st_crs(mask_ll))
      st_write(buf_inter_ll, "../../Urban/Shape/buf_inter.shp", delete_dsn=T)
      buf_outer_ll<-st_transform(buf_outer, crs=st_crs(mask_ll))
      st_write(buf_outer_ll, "../../Urban/Shape/buf_outer.shp", delete_dsn=T)
      
      plot(st_geometry(buf))
      plot(st_geometry(buf_inter), add=T, col="red")
      plot(st_geometry(buf_outer), add=T, col=as.factor(buf_outer$NAME10))
      
    }
    city_buffer[[as.character(dist)]]<-buf_outer
    item<-data.frame(city_area=area_no, buffer_area=as.numeric(st_area(buf_outer)),
                     city=city, buffer_dist=dist)
    if (is.null(all_buffer)){
      all_buffer<-item
    }else{
      all_buffer<-rbind(all_buffer, item)
    }
  }
  #saveRDS(city_buffer, target)
  if (F){
    st_area(city_buffer[["10000"]])
    st_area(city_buffer[["no"]])
    plot(st_geometry(city_buffer[["10000"]]))
    plot(st_geometry(city_buffer[["no"]]), add=T, col="red")
  }
}

dim(all_buffer)
all_buffer<-data.table(all_buffer)
all_buffer<-unique(all_buffer)
dim(all_buffer)
keycol <-c("city","buffer_dist")
setorderv(all_buffer, keycol)

all_buffer$ratio<-all_buffer$city_area/all_buffer$buffer_area
all_buffer$ratio_times<-abs(all_buffer$ratio - 1)

saveRDS(all_buffer, "../../Urban/Shape/City_buffer/buffer_info.rda")

all_buffer_min<-all_buffer[, .(ratio_times=min(ratio_times)), by="city"]
all_buffer_min$selected<-T
all_buffer<-merge(all_buffer, all_buffer_min, by=c("city", "ratio_times"))

saveRDS(all_buffer, "../../Urban/Shape/City_buffer/selected_buffer_info.rda")
all_buffer<-readRDS("../../Urban/Shape/City_buffer/selected_buffer_info.rda")

hist(all_buffer$ratio)
table(all_buffer$buffer_dist)
hist(all_buffer$buffer_dist)


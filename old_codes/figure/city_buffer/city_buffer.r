library(sf)
library(raster)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
all_buffer<-readRDS("../../Urban/Shape/City_buffer/selected_buffer_info.rda")
mask<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")
hist(all_buffer$ratio)

item<-all_buffer[ratio==max(ratio)]
item<-all_buffer[grepl("Atlanta, GA", city)]


city_name<-gsub("/", "-", (gsub(", ", "-", item$city)))
target<-sprintf("../../Urban/Shape/City_buffer/Buffers/%s.rda", city_name)
shapes<-readRDS(target)
city_shape<-shapes[["no"]]
buffer_shape<-shapes[[as.character(item$buffer_dist)]]


for (label in rev(names(shapes))){
  if (label=="10000"){
    plot(st_geometry(st_transform(shapes[[label]], crs=st_crs(mask_ll))))
  }else{
    if (item$buffer_dist==as.numeric(label)){
      plot(st_geometry(st_transform(shapes[[label]], crs=st_crs(mask_ll))), add=T, col="red")  
    }else{
      if (label=="no"){
        plot(st_geometry(st_transform(shapes[[label]], crs=st_crs(mask_ll))), add=T, col="blue")  
      }else{
        plot(st_geometry(st_transform(shapes[[label]], crs=st_crs(mask_ll))), add=T)  
      }
      
    }
    
  }
  
}
st_area(shapes[["500"]])
st_area(shapes[["no"]])

mean(all_buffer$ratio)
quantile(all_buffer$ratio, seq(from=0, to=1, by=0.25))

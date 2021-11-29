library(sf)
library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
#all_df<-readRDS("../../Urban/Tables/US_Year_with_native.rda")

mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")

polygon_folder<-"../../Shape/IUCN_Distribution_2021/st_simplify"
if (F){
  sp_list<-list.files("../../Shape/IUCN_Distribution_2021/RAW/")
  sp_list<-gsub("\\.rda", "", sp_list)
  saveRDS(sp_list, "../../Urban/Tables/birdlist_full_2021.rda")
}
sp_list<-readRDS("../../Urban/Tables/birdlist_full_2021.rda")
sp_list<-sp_list[sample(length(sp_list), length(sp_list))]
sp<-"Agelaius_tricolor"

sp_shape_list<-list()
sp_buffer_list<-list()
i=1
if (F){
  sp_shape_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_shape.rda")
  sp_buffer_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_buffer.rda")
  
}
for (i in c(1:length(sp_list))){
  sp<-sp_list[i]
  if (T){
    if (sp %in% names(sp_buffer_list)){
      next()
    }
  }
  print(paste(i, length(sp_list), sp))
  t<-sprintf("%s/%s.rda", polygon_folder, sp)
  if (!file.exists(t)){
    next()
  }
  occ_shape<-readRDS(t)
  if (nrow(occ_shape)==0){
    next()
  }
  
  occ_shape_sinu<-st_transform(occ_shape, crs=st_crs(mask_10km))

  #range_box<-st_bbox(occ_shape_sinu)
  #dist<-sqrt((range_box[1]-range_box[3])^2+(range_box[2]-range_box[4])^2)
  dist<-sqrt(sum(st_area(occ_shape_sinu))/pi)
  dist_p<-0.1
  occ_shape_sinu_buffer<-list()
  for (dist_p in c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01)){
    item<-st_buffer(occ_shape_sinu, dist * dist_p)  
    occ_shape_sinu_buffer[[as.character(dist_p)]]<-item
    if(F){
      plot(st_geometry(item), add=T)
    }
  }
  
  
  #occ_shape_ll_buffer<-st_transform(occ_shape_sinu_buffer, crs=st_crs(mask_ll))
  #occ_shape_ll<-st_transform(occ_shape, crs=st_crs(mask_ll))
  sp_shape_list[[sp]]<-occ_shape_sinu
  sp_buffer_list[[sp]]<-occ_shape_sinu_buffer
}
saveRDS(sp_shape_list, "../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_shape.rda")
saveRDS(sp_buffer_list, "../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_buffer.rda")
length(sp_shape_list)
length(sp_buffer_list)

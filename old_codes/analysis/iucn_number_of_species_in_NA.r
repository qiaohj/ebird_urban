library(raster)
library(sf)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
library(sf)
library(data.table)
library(raster)
rm(list=ls())
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
polygon_folder<-"/media/huijieqiao/Speciation_Extin/Sp_Richness_GCM/Objects/IUCN_Distribution/Birds/st_simplify"
sp_list<-readRDS("../../Urban/Tables/birdlist.rda")
sp<-sp_list[1]
#sp<-"Turdus_migratorius"
sp<-"Acrocephalus_astrolabii"
#Caprimulgus_centralasicus, Petrochelidon_perdita
mask<-raster("../../Urban/Raster/mask.tif")
sp_all<-list()
i=1
for (i in c(1:length(sp_list))){
  sp<-sp_list[i]
  print(paste(i, length(sp_list), sp))
  
  occ_shape<-readRDS(sprintf("%s/%s.rda", polygon_folder, sp))
  occ_shape<-st_transform(occ_shape, crs=st_crs(mask))
  if (nrow(occ_shape)==0){
    next()
  }
  occ_shape <- occ_shape[!st_is_empty(occ_shape),,drop=FALSE]
  if (nrow(occ_shape)==0){
    next()
  }
  item_crop<-crop(mask, occ_shape)
  
  item_crop<-mask(item_crop, occ_shape)
  item_p<-data.table(rasterToPoints(item_crop))
  item_p$species<-sp
  sp_all[[sp]]<-item_p
  
}
sp_all<-rbindlist(sp_all)
saveRDS(sp_all, "../../Urban/Tables/IUCN_Raster_raw.rda")

sp_all<-readRDS("../../Urban/Tables/IUCN_Raster_raw.rda")
mask_index<-mask
values(mask_index)[!is.na(values(mask_index))]<-c(1:length(values(mask_index)[!is.na(values(mask_index))]))
plot(mask_index)
cols<-c("x", "y")
sp_all$index<-raster::extract(mask_index, sp_all[, ..cols])
sp_all_N<-sp_all[, .(N=.N), by=list(index)]

mask_p<-data.table(rasterToPoints(mask_index))

points<-merge(mask_p, sp_all_N, by.x="mask", by.y="index", all.x=T, all.y=F)


setorder(points, mask)

r<-mask
values(r)[!is.na(values(r))]<-points$N
plot(r)
writeRaster(r, "../../Urban/Raster/IUCN_Species_Richness.tif", overwrite=T)

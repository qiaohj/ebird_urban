library(sf)
library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
mask<-raster("../../Urban/Raster/mask_10km.tif")
mask_ll<-raster("../../Urban/Raster/mask.tif")

all_item<-readRDS("../../Urban/Tables/US_city_iucn_range_map_result.rda")

if (F){
  xy_cols<-ll_cols<-c("centroid_x", "centroid_y")
  points<-st_as_sf(all_item, coords = xy_cols, crs=st_crs(mask))
  
  points<-st_transform(points, crs=st_crs(mask_ll))
  coords<-st_coordinates(points)
  all_item$centroid_lon<-coords[,1]
  all_item$centroid_lat<-coords[,2]
  saveRDS(all_item, "../../Urban/Tables/US_city_iucn_range_map_result.rda")
}

ggplot(all_item)+
  geom_point(aes(x=centroid_lat, y=city_sp_count), col="red")+
  geom_point(aes(x=centroid_lat, y=city_sp_buffer_count), col="blue")

ggplot(all_item)+
  geom_line(aes(x=centroid_lat, y=city_buffer_sp_count), col="red")+
  geom_line(aes(x=centroid_lat, y=city_buffer_sp_buffer_count), col="blue")

all_item_ebird<-readRDS("../../Urban/Tables/US_city_result.rda")
if (F){
  xy_cols<-ll_cols<-c("CENTROID_X", "CENTROID_Y")
  points<-st_as_sf(all_item_ebird, coords = xy_cols, crs=st_crs(mask))
  
  points<-st_transform(points, crs=st_crs(mask_ll))
  coords<-st_coordinates(points)
  all_item_ebird$CENTROID_LON<-coords[,1]
  all_item_ebird$CENTROID_LAT<-coords[,2]
  saveRDS(all_item_ebird, "../../Urban/Tables/US_city_result.rda")
}

ggplot(all_item_ebird[(!is.na(NATIVE))&(TYPE!="ALL-NATIVE-1")])+
  geom_point(aes(x=CENTROID_LAT, y=N_SPECIES, col=NATIVE), alpha=0.2, size=0.1)+
  geom_smooth(aes(x=CENTROID_LAT, y=N_SPECIES, col=NATIVE))+
  facet_wrap(~TYPE, nrow=2)+
  theme_bw()
unique(all_item_ebird$NATIVE2)
unique(all_item_ebird$TYPE)

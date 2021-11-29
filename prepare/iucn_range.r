library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(gdalUtilities)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
setDTthreads(20)
print(sprintf("Current core number is %d", getDTthreads()))
mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
if (F){
  f1<-list.files("../../Shape/IUCN_Distribution_2021/st_simplify")
  f2<-list.files("../../Shape/IUCN_Distribution_2021/RAW")
  sps<-f2[!(f2 %in% f1)]
  sps<-gsub("_", " ", sps)
  sps<-gsub("\\.rda", "", sps)
  for (sp in sps){
    tmp_sf <- readRDS(sprintf("../../Shape/IUCN_Distribution_2021/RAW/%s.rda", gsub(" ", "_", sp)))
    if (nrow(tmp_sf)>1){
      adf
    }
  }
  st_union(tmp_sf[1,], tmp_sf[2,])
}

ensure_multipolygons(tmp_sf)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

if (F){
  vessel <- sf::st_read(dsn = "/media/huijieqiao/WD12T/GISLayers/IUCN_Rangemap/2021/BOTW/BOTW.gdb", 
                        layer = "All_Species")
  sp_df_eck4<-st_transform(vessel, crs = st_crs(mask_10km))
  
  vessel[which(vessel$binomial=="Arremonops conirostris"),]
  
  vessel[1,]
  unique <- unique(vessel$binomial)
  unique<-as.character(unique)
  i=10
  bi<-"Agelasticus cyanopus"
  for (i in 1:length(unique)) {
    bi<-unique[i]
    print(paste(i, length(unique), bi))
    #tmp_sf <- sp_df_eck4[which((bird_df$binomial == bi)&(bird_df$PRESENCE %in% PRESENCE)&
    #                             (bird_df$ORIGIN %in% ORIGIN)&(bird_df$SEASONAL %in% SEASONAL)), ]
    ff_name<-sprintf("../../Shape/IUCN_Distribution_2021/st_simplify/%s.rda", gsub(" ", "_", bi))
    if (file.exists(ff_name)){
      next()
    }
    tmp_sf<-sp_df_eck4[which(sp_df_eck4$binomial == bi), ]
    #tmp_sf<-readRDS(sprintf("../../Shape/IUCN_Distribution_2021/RAW/%s.rda", gsub(" ", "_", bi)))
    saveRDS(tmp_sf, sprintf("../../Shape/IUCN_Distribution_2021/RAW/%s.rda", gsub(" ", "_", bi)))
    #tryCatch(
    #  {
        if (st_geometry_type(tmp_sf)=="MULTISURFACE"){
          tmp_sf_sim<-ensure_multipolygons(tmp_sf)
        }else{
          tmp_sf_sim<-st_cast(tmp_sf, "MULTIPOLYGON")
        }
        
        tmp_sf_sim<-st_simplify(tmp_sf_sim, dTolerance = 5000)
        saveRDS(tmp_sf_sim, sprintf("../../Shape/IUCN_Distribution_2021/st_simplify/%s.rda", gsub(" ", "_", bi)))
        #tmp_sf_rm_sim<-rmapshaper::ms_simplify(tmp_sf, keep=0.01)
        #saveRDS(tmp_sf_rm_sim, sprintf("../../Shape/IUCN_Distribution_2021/ms_simplify/%s.rda", gsub(" ", "_", bi)))
    #},
    #error=function(cond) {
        
    #}
    #)
    
  }
}

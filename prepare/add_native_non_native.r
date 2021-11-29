library(sf)
library(data.table)
library(raster)
rm(list=ls())
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
polygon_folder<-"../../Shape/IUCN_Distribution/st_simplify"

if (F){
  sp_list<-list.files(polygon_folder, pattern="\\.rda")
  sp_list<-gsub("\\.rda", "", sp_list)
  saveRDS(sp_list, "../../Urban/Tables/birdlist.rda")
}
sp_list<-readRDS("../../Urban/Tables/birdlist.rda")
occ_folder<-"/media/huijieqiao/WD12T/eBird/Tables/Species_Split_Origin"
sp<-sp_list[1]
#sp<-"Turdus_migratorius"
sp<-"Buteo_plagiatus"
#Caprimulgus_centralasicus, Petrochelidon_perdita
mask<-raster("../../Urban/Raster/mask.tif")
mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
sp_list<-sp_list[sample(length(sp_list), length(sp_list))]
sp_shapes<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_shape.rda")
sp_buffers<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_buffer.rda")
i=1
for (i in c(1:length(sp_list))){
  sp<-sp_list[i]
  print(paste(i, length(sp_list), sp))
  target<-sprintf("../../Urban/Tables/Occurrences/%s.rda", sp)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  
  if (!file.exists(sprintf("%s/%s.rda", occ_folder, sp))){
    next()
  }
  occ<-readRDS(sprintf("%s/%s.rda", occ_folder, sp))
  if (is.null(occ)){
    next()
  }
  if (nrow(occ)==0){
    next()
  }
  occ<-data.table(occ)
  cols<-c("TAXONOMIC_ORDER", "CATEGORY", "COMMON NAME", "SCIENTIFIC NAME",
          "OBSERVATION COUNT", "COUNTRY", "STATE", "COUNTY", "COUNTY CODE", 
          "LOCALITY", "LOCALITY ID", "LOCALITY TYPE", "LATITUDE", "LONGITUDE",
          "OBSERVATION DATE", "OBSERVER ID", "DURATION MINUTES", "EFFORT DISTANCE KM",
          "APPROVED", "REVIEWED")
  occ<-occ[, ..cols]
  colnames(occ)<-c("TAXONOMIC_ORDER", "CATEGORY", "COMMON_NAME", "SCIENTIFIC_NAME",
                   "OBSERVATION_COUNT", "COUNTRY", "STATE", "COUNTY", "COUNTY_CODE", 
                   "LOCALITY", "LOCALITY_ID", "LOCALITY_TYPE", "LATITUDE", "LONGITUDE",
                   "OBSERVATION_DATE", "OBSERVER_ID", "DURATION_MINUTES", "EFFORT_DISTANCE_KM",
                   "APPROVED", "REVIEWED")
  occ$OBSERVATION_DATE<-as.POSIXct(strptime(occ$OBSERVATION_DATE, format ="%Y-%m-%d"))
  occ<-occ[!is.na(OBSERVATION_DATE)]
  occ$YEAR<-as.numeric(format(occ$OBSERVATION_DATE, "%Y"))
  occ$MONTH<-as.numeric(format(occ$OBSERVATION_DATE, "%m"))
  occ$DAY<-as.numeric(format(occ$OBSERVATION_DATE, "%d"))
  occ$DAYS<-as.numeric(abs(difftime(strptime("1980-01-01", format = "%Y-%m-%d"),
                                   strptime(occ$OBSERVATION_DATE,
                                            format = "%Y-%m-%d"), 
                                   units="days")))
  occ$OBSERVATION_COUNT<-as.numeric(occ$OBSERVATION_COUNT)
  occ$DAYS_OF_YEAR<-as.numeric(strftime(occ$OBSERVATION_DATE, format = "%j"))
  occ[is.na(OBSERVATION_COUNT), "OBSERVATION_COUNT"]<-1
  
  occ$LATITUDE<-as.numeric(occ$LATITUDE)
  occ$LONGITUDE<-as.numeric(occ$LONGITUDE)
  occ$APPROVED<-as.numeric(occ$APPROVED)
  occ$REVIEWED<-as.numeric(occ$REVIEWED)
  occ$EFFORT_DISTANCE_KM<-as.numeric(occ$EFFORT_DISTANCE_KM)
  ll_cols<-c("LONGITUDE", "LATITUDE")
  occ_points<-st_as_sf(occ[, ..ll_cols], coords = ll_cols, crs=st_crs(mask))
  occ_points<-st_transform(occ_points, crs=st_crs(mask_10km))
  
  
  occ_shape<-sp_buffers[[sp]]
  occ_shape[["0"]]<-sp_shapes[[sp]]
  buffer<-"0"
  
  occ$NATIVE<-"NON-NATIVE"
  occ$SEASONAL<-0
  
  for (buffer in names(occ_shape)){
    item_shape<-occ_shape[[buffer]]
    #Presence, 1:Extant, 2:Probably Extant, 3:Possibly Extant, 4:Possibly Extinct, 
    #5:Extinct, 6:Presence Uncertain
    
    #Origin Codes, 1: Native, 2:Reintroduced, 3:Introduced, 4:Vagrant,
    #5:Origin Uncertain, 6:Assisted Colonisation
    
    #Seasonality Codes, 1:Resident, 2:Breeding Season, 3:Non-breeding Season,
    #4:Passage, 5:Seasonal Occurrence Uncertain
    
    item_shape<-item_shape[which(item_shape$ORIGIN %in% c(1:2)),]
    if (nrow(item_shape)==0){
      next()
    }
    item_shape<-item_shape[which(item_shape$SEASONAL %in% c(1:3)),]
    if (nrow(item_shape)==0){
      next()
    }
    item_shape<-st_transform(item_shape, crs=st_crs(mask_10km))
    iii=1
    for (iii in c(1:nrow(item_shape))){
      subitem_shape<-item_shape[iii,]
      index<-st_contains(item_shape, occ_points)
      indics<-c()
      for (j in c(1:length(index))){
        indics<-c(indics, index[[j]])
      }
      
      occ[indics, "NATIVE"]<-buffer
      occ[indics, "SEASONAL"]<-subitem_shape$SEASONAL
    }
    
  }
  occ$SEASONAL_MONTH<-ifelse(occ$SEASONAL==0, 0, 1)
  occ[(MONTH %in% c(6, 7))&(SEASONAL==3)]$SEASONAL_MONTH<-0
  occ[(MONTH %in% c(1, 12))&(SEASONAL==2)]$SEASONAL_MONTH<-0
  
  saveRDS(occ, target)
  if (F){
    colors<-rainbow(9)
    names(colors)<-names(occ_shape)
    
    ll_cols<-c("LONGITUDE", "LATITUDE")
    #occ_points<-st_point(as.matrix(occ[, ..ll_cols]))
    #occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), 71), ], coords = ll_cols, crs=st_crs(mask))
    occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), nrow(occ)), ], 
                                 coords = ll_cols, crs=st_crs(mask))
    occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), 1000), ], 
                                 coords = ll_cols, crs=st_crs(mask))
    for (j in c(1:length(names(occ_shape)))){
      lab<-names(occ_shape)[j]
      if (j==1){
        plot(st_geometry(st_transform(occ_shape[[lab]], crs=st_crs(mask))))
      }else{
        plot(st_geometry(st_transform(occ_shape[[lab]], crs=st_crs(mask))), add=T)
      }
      
    }
    plot(st_geometry(occ_points_sampled), 
         col=colors[occ_points_sampled$NATIVE], add=T, pch=3)
  }
}

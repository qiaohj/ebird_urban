library(data.table)
library(sf)
library(raster)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
dirs<-list.dirs("../../Tables/Locality_202105/United States")
d<-dirs[length(dirs-1)]
all_items<-list()
for (d in dirs){
  target<-sprintf("%s/raw.rda", d)
  if (!file.exists(target)){
    next()
  }
  print(target)
  item<-readRDS(target)
  all_items[[d]]<-item
}

all_items<-rbindlist(all_items)

all_items_se_year<-all_items[, .(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
                                 EVENT_COUNT=.N,
                                 OBSERVER_COUNT=length(unique(OBSERVER_ID))),
                             by=list(LOCALITY_ID, COUNTRY, STATE, COUNTY, 
                                     LATITUDE, LONGITUDE, SCIENTIFIC_NAME, 
                                     YEAR, X, Y, INDEX_2500m, INDEX_5km, INDEX_10km)]


saveRDS(all_items_se_year, "../../Urban/Tables/US_Year.rda")

all_items_se_all<-all_items[YEAR>=2015, .(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
                                          EVENT_COUNT=.N,
                                          OBSERVER_COUNT=length(unique(OBSERVER_ID))),
                            by=list(LOCALITY_ID, COUNTRY, STATE, COUNTY, 
                                    LATITUDE, LONGITUDE, SCIENTIFIC_NAME, 
                                    X, Y, INDEX_2500m, INDEX_5km, INDEX_10km)]


saveRDS(all_items_se_all, "../../Urban/Tables/US_ALL_After_2015.rda")


all_items_se_month<-all_items[, .(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
                                  EVENT_COUNT=.N,
                                  OBSERVER_COUNT=length(unique(OBSERVER_ID))),
                              by=list(LOCALITY_ID, COUNTRY, STATE, COUNTY, 
                                      LATITUDE, LONGITUDE, SCIENTIFIC_NAME, 
                                      YEAR, MONTH, X, Y, INDEX_2500m, INDEX_5km, INDEX_10km)]


saveRDS(all_items_se_month, "../../Urban/Tables/US_Year_MONTH.rda")

all_items_se_month_all<-all_items[YEAR>=2015, .(OBSERVATION_COUNT=sum(OBSERVATION_COUNT),
                                  EVENT_COUNT=.N,
                                  OBSERVER_COUNT=length(unique(OBSERVER_ID))),
                              by=list(LOCALITY_ID, COUNTRY, STATE, COUNTY, 
                                      LATITUDE, LONGITUDE, SCIENTIFIC_NAME, 
                                      MONTH, X, Y, INDEX_2500m, INDEX_5km, INDEX_10km)]


saveRDS(all_items_se_month_all, "../../Urban/Tables/US_MONTH_After_2015.rda")



#all_items_se<-readRDS("../../Urban/Tables/US_ALL_After_2015.rda")
all_items_se<-readRDS("../../Urban/Tables/US_MONTH_After_2015.rda")
sp_list<-unique(all_items_se$SCIENTIFIC_NAME)
sp<-sp_list[1]
#Caprimulgus_centralasicus, Petrochelidon_perdita
mask<-raster("../../Urban/Raster/mask.tif")
mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
polygon_folder<-"/media/huijieqiao/Speciation_Extin/Sp_Richness_GCM/Objects/IUCN_Distribution/Birds/st_simplify"
iiiii=1
sp_list<-sp_list[sample(length(sp_list), length(sp_list))]
sp_shapes<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_shape.rda")
sp_buffers<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer/sp_buffer.rda")

all_df<-list()
for (iiiii in c(1:length(sp_list))){
  sp<-sp_list[iiiii]
  print(paste(iiiii, length(sp_list), sp))
  sp_label<-gsub(" ", "_", sp)
  if (!file.exists(sprintf("%s/%s.rda", polygon_folder, sp_label))){
    next()
  }
  occ_shape<-sp_shapes[[sp_label]]
  if (nrow(occ_shape)==0){
    next()
  }
  occ_buffer<-sp_buffers[[sp_label]]
  occ_buffer[["0"]]<-occ_shape
  
  occ<-all_items_se[SCIENTIFIC_NAME==sp]
  
  ll_cols<-c("LONGITUDE", "LATITUDE")
  occ_points<-st_as_sf(occ, coords = ll_cols, crs=st_crs(mask))
  occ_points<-st_transform(occ_points, crs=st_crs(mask_10km))
  occ$NATIVE<-"NON-NATIVE"
  
  occ$SEASONAL<-0
  buffer<-"0"
  for (buffer in names(occ_buffer)){
    item_shape<-occ_buffer[[buffer]]
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
  
  all_df[[sp]]<-occ
  if (F){
    colors<-rainbow(9)
    names(colors)<-c("NON-NATIVE", names(occ_buffer))
    
    ll_cols<-c("LONGITUDE", "LATITUDE")
    #occ_points<-st_point(as.matrix(occ[, ..ll_cols]))
    #occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), 71), ], coords = ll_cols, crs=st_crs(mask))
    occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), nrow(occ)), ], 
                                 coords = ll_cols, crs=st_crs(mask))
    occ_points_sampled<-st_as_sf(occ[sample(nrow(occ), 1000), ], 
                                 coords = ll_cols, crs=st_crs(mask))
    for (j in c(1:length(names(occ_buffer)))){
      lab<-names(occ_buffer)[j]
      if (j==1){
        plot(st_geometry(st_transform(occ_buffer[[lab]], crs=st_crs(mask))))
      }else{
        plot(st_geometry(st_transform(occ_buffer[[lab]], crs=st_crs(mask))), add=T)
      }
      
    }
    plot(st_geometry(occ_points_sampled), 
         col=colors[occ_points_sampled$NATIVE], add=T, pch=3)
  }
}

all_df<-rbindlist(all_df)
saveRDS(all_df, "../../Urban/Tables/US_MONTH_After_2015_with_native.rda")

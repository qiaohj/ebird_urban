library(data.table)
library(sf)
library(raster)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
if (F){
  #check flag
  error_list<-list()
  for (year in c(2015:2019)){
    print(year)
    flag<-read.csv(sprintf("../../Urban/Tables/Marked_Checklist/flg_us_%d.txt", year), 
                   stringsAsFactors=F)
    flag<-data.table(flag)
    cols<-c("STATE", "COUNTY", "LOCALITY", "flg_key", "flg_ply", "flg_ply1", "flg_ply2")
    flag<-flag[, ..cols]
    flag<-unique(flag)
    flag_se<-flag[, .(N=.N), by=list(STATE, COUNTY, LOCALITY)]
    flag_se<-flag_se[N>1]
    flag_item<-flag[LOCALITY %in% flag_se$LOCALITY]
    flag_item$YEAR<-year
    error_list[[as.character(year)]]<-flag_item
  }
  error_list<-rbindlist(error_list)
  write.csv(error_list, "../../Urban/Tables/Marked_Checklist/flg_us_error.csv", row.names=F)
}
if (F){
  #handle the match of IUCN and eBird checklist
  
  checklist<-read.csv("../../Urban/Tables/Marked_Checklist/9_merged_na_ebird_iucn_v2021.csv", 
                      stringsAsFactors=F)
  #checklist$name_ebird_2<-gsub("_", " ", checklist$name_ebird)
  
  head(checklist)
  checklist<-data.table(checklist)
  
  checklist[name_ebird=="Psittacara_holochlorus"]$name_iucn<-"Psittacara_holochlorus"
  checklist[name_iucn=="Hapalocrex_flaviventer"]$name_iucn<-"Laterallus_flaviventer"
  checklist[name_iucn=="Laterallus flaviventer"]$name_iucn<-"Laterallus_flaviventer"
  checklist[name_iucn=="Lepidopyga_coeruleogularis"]$name_iucn<-"Amazilia_coeruleogularis"
  checklist[name_iucn=="Psittacula_krameri"]$name_iucn<-"Alexandrinus_krameri"
  checklist[name_iucn=="Hylocharis_eliciae"]$name_iucn<-"Amazilia_eliciae"
  checklist[name_iucn=="Juliamyia_julie"]$name_iucn<-"Amazilia_eliciae"
  
  
  dim(checklist)
  checklist<-unique(checklist)
  iucn_name<-checklist$name_iucn[1]
  group_index<-1
  checklist$group_name<-""
  for (iucn_name in unique(checklist$name_iucn)){
    item<-checklist[name_iucn==iucn_name]
    if (item[1]$group_name!=""){
      next()
    }
    ebird_names<-unique(item$name_ebird)
    iucn_names<-unique(checklist[name_ebird %in% ebird_names]$name_iucn)
    ebird_names<-unique(checklist[name_iucn %in% iucn_names]$name_ebird)
    if (length(iucn_names)>1){
      group_name<-sprintf("Sp_group_%d", group_index)
      group_index<-group_index+1
    }else{
      group_name<-iucn_names[1]
    }
    checklist[(name_iucn %in% iucn_names)|(name_ebird %in% ebird_names)]$group_name<-group_name
  }
  cols<-c("name_ebird", "name_iucn", "group_name")
  checklist<-checklist[, ..cols]
  checklist$name_ebird_2<-gsub("_", " ", checklist$name_ebird)
  checklist$name_iucn_2<-gsub("_", " ", checklist$name_iucn)
  saveRDS(checklist, "../../Urban/Tables/cleaned_checklist.rda")
  checklist[name_iucn=="Xiphorhynchus_aequatorialis"]
  checklist[name_ebird=="Xiphorhynchus_erythropygius"]
  checklist[group_name=="Sp_group_63"]
  colnames(checklist)
  checklist<-checklist[name_ebird!=name_iucn]
  table(checklist$name_ebird)
  checklist$name_group<-""
  
  checklist[name_ebird=="Passerella_iliaca"]
  checklist[name_iucn %in% checklist[name_ebird=="Passerella_iliaca"]$name_iucn]
  checklist[name_iucn %in% name_ebird]
  checklist[name_ebird %in% name_iucn]
  checklist[name_ebird %in% c("Psittacara_holochlorus")]
  checklist[name_iucn %in% c("Psittacara_holochlorus")]
  
}
if (F){
  sp_shape_list<-list.files("/Users/huijieqiao/media/huijieqiao/WD12T/eBird/Shape/IUCN_Distribution_2021/st_simplify")
  sp_shape_list<-gsub("\\.rda", "", sp_shape_list)
  #merge iucn range map
  checklist<-readRDS("../../Urban/Tables/cleaned_checklist.rda")
  #Laterallus flaviventer
  rm("group_name")
  iucn_range<-list()
  g_name<-"Sp_group_1"
  dd<-checklist[!(name_iucn %in% sp_shape_list)]

  write.csv(dd, "~/missing_species_iucn.csv", row.names=F)
}
if (F){
  df<-readRDS("/media/huijieqiao/WD12T/eBird/Urban/Tables/old/birdlist_full_2021.rda")
  #merge all data we need for the analysis to make the dataset smaller
  df_all<-list()
  for (year in c(2015:2019)){
    print(sprintf("Reading %d", year))
    f<-sprintf(sprintf("../../Urban/US_Raw/Years/%d.rda", year))
    df<-readRDS(f)
    df<-rbindlist(df)
    df_all[[as.character(year)]]<-df
  }
  df_all<-rbindlist(df_all)
  names(df_all)
  
  checklist<-read.csv("../../Urban/Tables/Marked_Checklist/9_merged_na_ebird_iucn_v2021.csv", 
                      stringsAsFactors=F)
  checklist$name_ebird_2<-gsub("_", " ", checklist$name_ebird)
  df_all2<-df_all[SCIENTIFIC_NAME %in% checklist$name_ebird_2]
  
  names(df_all2)
  df_all2$OBSERVATION_COUNT_INT<-as.numeric(df_all2$OBSERVATION_COUNT)
  df_all2[is.na(OBSERVATION_COUNT_INT)]$OBSERVATION_COUNT_INT<-0
  if (F){
    #merge flag
    df_all_list<-list()
    year<-2019
    for (year in c(2015:2019)){
      print(sprintf("Merging flag @ year %d", year))
      flag<-read.csv(sprintf("../../Urban/Tables/Marked_Checklist/flg_us_%d.txt", year), 
                     stringsAsFactors=F)
      flag<-data.table(flag)
      cols<-c("STATE", "COUNTY", "LOCALITY", "flg_key", "flg_ply", "flg_ply1", "flg_ply2")
      flag<-flag[, ..cols]
      flag<-unique(flag)
      flag<-flag[, .(flg_key=max(flg_key), flg_ply=max(flg_ply), 
                     flg_ply1=max(flg_ply1), flg_pl2=max(flg_ply2)),
                 by=list(STATE, COUNTY, LOCALITY)]
      flag$YEAR<-year
      df_all_item<-df_all2[YEAR==year]
      #dim(df_all_item)
      df_all_item<-merge(df_all_item, flag,  by=c("STATE", "COUNTY", "LOCALITY", "YEAR"), all.x=T, all.y=F)
      #dim(df_all_item)
      df_all_list[[as.character(year)]]<-df_all_item
    }
    df_all_list<-rbindlist(df_all_list)
    
    saveRDS(df_all_list, "../../Urban/Tables/Formatted_eBird/merged_zoo_flag.rda")
    df_all_list<-readRDS("../../Urban/Tables/Formatted_eBird/merged_zoo_flag.rda")
    df_all_se<-df_all_list[, .(N_Events=.N, 
                               OBSERVATION_COUNT=sum(OBSERVATION_COUNT_INT)),
                           by=list(STATE, COUNTY, LOCALITY, SCIENTIFIC_NAME,
                                   LATITUDE, LONGITUDE, MONTH, 
                                   flg_key, flg_ply, flg_ply1, flg_pl2)]
    saveRDS(df_all_se, "../../Urban/Tables/Formatted_eBird/merged_by_year.rda")
  }
}
if (F){
  #add the iucn_in_out information based on the iucn range map and buffer.
  #table 4 PRESENCE: 1. Extant. 2. Probably Extant. 3. Possibly Extant 
  #4. Possibly Extinct 5. Extinct. 6. Presence Uncertain
  #table 5 ORIGIN: 1. Native. 2. Reintroduced 3. Introduced. 4. Vagrant
  #5. Origin Uncertain 6. Assisted Colonisation
  #table 6 SEASONALITY: 1. Resident 2. Breeding Season 3. Non-breeding Season 
  #4. Passage 5. Seasonal Occurrence Uncertain
  
  #flag_in_out_all: Table 5: (1, 2), and Table 6 (1, 2, 3), all months
  #flag_in_out_summer: Table 5: (1, 2), and Table 6 (1, 2), June and July
  #flag_in_out_winter: Table 5: (1, 2), and Table 6 (1, 3), Jan and Dec
  mask<-raster("../../Urban/Raster/mask_10km.tif")
  mask_ll<-raster("../../Urban/Raster/mask.tif")
  
  df_all_se<-readRDS("../../Urban/Tables/Formatted_eBird/merged_by_year.rda")
  checklist<-readRDS("../../Urban/Tables/cleaned_checklist.rda")
  group<-"Sp_group_1"
  df_all_se$flag_in_out_all<-0
  df_all_se$flag_in_out_summer<-0
  df_all_se$flag_in_out_winter<-0
  df_all_se$flag_in_out_all_buffer_0.01<-0
  df_all_se$flag_in_out_summer_buffer_0.01<-0
  df_all_se$flag_in_out_winter_buffer_0.01<-0
  df_all_se$flag_in_out_all_buffer_0.1<-0
  df_all_se$flag_in_out_summer_buffer_0.1<-0
  df_all_se$flag_in_out_winter_buffer_0.1<-0
  df_all_se$flag_in_out_all_buffer_0.05<-0
  df_all_se$flag_in_out_summer_buffer_0.05<-0
  df_all_se$flag_in_out_winter_buffer_0.05<-0
  
  df_results<-list()
  for (group in unique(checklist$group_name)){
    print(group)
    if (group %in% names(df_results)){
      next()
    }
    item<-checklist[group_name==group]
    ebird<-df_all_se[SCIENTIFIC_NAME %in% item$name_ebird_2]
    if (nrow(ebird)==0){
      next()
    }
    ll_cols<-c("LONGITUDE", "LATITUDE")
    localities_points<-st_as_sf(ebird, coords = ll_cols, crs=st_crs(mask_ll))
    localities_points<-st_transform(localities_points, crs=st_crs(mask))
    i=1
    
    for (i in c(1:nrow(item))){
      if (item[i]$name_iucn %in% c("___notFound__")){
        next()
      }
      shape<-readRDS(sprintf("../../Shape/IUCN_Distribution_2021/st_simplify/%s.rda", 
                     item[i]$name_iucn))
      shape<-shape[which(shape$origin %in% c(1, 2)),]
      shape<-shape[which(shape$seasonal %in% c(1, 2, 3)),]
      if (nrow(shape)>0){
        flag_in_out_all_index<-unique(unlist(st_contains(shape, localities_points)))
        ebird[flag_in_out_all_index]$flag_in_out_all<-
          ebird[flag_in_out_all_index]$flag_in_out_all + 1
      }
      
      shape_summer<-shape[which(shape$seasonal %in% c(1, 2)),]
      if (nrow(shape_summer)>0){
        flag_in_out_summer_index<-unique(unlist(st_contains(shape_summer, localities_points)))
        ebird[flag_in_out_summer_index]$flag_in_out_summer<-
          ebird[flag_in_out_summer_index]$flag_in_out_summer + 1
      }
      
      shape_winter<-shape[which(shape$seasonal %in% c(1, 3)),]
      if (nrow(shape_winter)>0){
        flag_in_out_winter_index<-unique(unlist(st_contains(shape_winter, localities_points)))
        ebird[flag_in_out_winter_index]$flag_in_out_winter<-
          ebird[flag_in_out_winter_index]$flag_in_out_winter + 1
      }
      
      buffer<-readRDS(sprintf("../../Shape/IUCN_Distribution_2021/buffer/%s.rda", 
                             item[i]$name_iucn))
      buf_size<-0.01
      for (buf_size in c(0.01, 0.05, 0.1)){
        buffer_item<-buffer[[as.character(buf_size)]]
        buffer_item<-buffer_item[which(buffer_item$origin %in% c(1, 2)),]
        buffer_item<-buffer_item[which(buffer_item$seasonal %in% c(1, 2, 3)),]
        if (nrow(buffer_item)>0){
          flag_in_out_all_index<-unique(unlist(st_contains(buffer_item, localities_points)))
          colname<-sprintf("flag_in_out_all_buffer_%s", as.character(buf_size))
          ebird[flag_in_out_all_index, (colname):=ebird[flag_in_out_all_index, get(colname)]+1]
        }
        
        
        buffer_item_summer<-buffer_item[which(buffer_item$seasonal %in% c(1, 2)),]
        if (nrow(buffer_item_summer)>0){
          flag_in_out_summer_index<-unique(unlist(st_contains(buffer_item_summer, localities_points)))
          colname<-sprintf("flag_in_out_summer_buffer_%s", as.character(buf_size))
          ebird[flag_in_out_summer_index, (colname):=ebird[flag_in_out_summer_index, get(colname)]+1]
        }
        buffer_item_winter<-buffer_item[which(buffer_item$seasonal %in% c(1, 3)),]
        if (nrow(buffer_item_winter)>0){
          flag_in_out_winter_index<-unique(unlist(st_contains(buffer_item_winter, localities_points)))
          colname<-sprintf("flag_in_out_winter_buffer_%s", as.character(buf_size))
          ebird[flag_in_out_winter_index, (colname):=ebird[flag_in_out_winter_index, get(colname)]+1]
        }
        
      }
      if (F){
        colors<-rainbow(3)
        box<-st_bbox(xx_points)
        box2<-st_bbox(buffer_item)
        box<-data.frame(x=c(min(box[1], box2[1]), max(box[3], box2[3])), 
                        y=c(min(box[2], box2[2]), max(box[4], box2[4])))
        plot(box, pch=".", col="white")
        plot(st_geometry(shape), border=colors[shape$origin], add=T)
        plot(st_geometry(shape_summer), border=colors[shape_summer$seasonal], add=T)
        plot(st_geometry(shape_winter), border=colors[shape_winter$seasonal], add=T)
        plot(st_geometry(buffer_item_summer), border=colors[buffer_item_summer$seasonal], add=T)
        plot(st_geometry(buffer_item_winter), border=colors[buffer_item_winter$seasonal], add=T)
        xx_points<-st_as_sf(ebird, coords = ll_cols, crs=st_crs(mask_ll))
        xx_points<-st_transform(xx_points, crs=st_crs(mask))
        plot(st_geometry(xx_points[which(xx_points$flag_in_out_summer_buffer_0.1>0),]), add=T, pch=".", col="green")
        plot(st_geometry(xx_points[which(xx_points$flag_in_out_winter_buffer_0.1>0),]), add=T, pch=".", col="red")
        plot(st_geometry(xx_points[which(xx_points$flag_in_out_all_buffer_0.1==0),]), add=T, pch=".", col="black")
        
      }
    }
    ebird$GROUP_NAME<-group
    df_results[[paste(group)]]<-ebird
  }
  df_results_list<-rbindlist(df_results)
  saveRDS(df_results_list, "../../Urban/Tables/flagged_data.rda")
}

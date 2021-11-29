library(sf)
library(data.table)
library(raster)
library(RMySQL)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
sp_shape_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_shape.rda")
continents<-st_read("../../Urban/Shape/north_america_continent/north_america_continent.shp")
mask<-raster("../../Urban/Raster/mask.tif")
mask_10km<-raster("../../Urban/Raster/mask_10km.tif")
st_crs(continents)<-st_crs(mask)
continents<-st_transform(continents, crs=st_crs(sp_shape_list[[1]]))

na<-continents
#st_crs(na)<-st_crs(mask)
#na<-st_transform(na, crs=st_crs(sp_shape_list[[1]]))

plot(st_geometry(continents))
plot(st_geometry(na), add=T, col="grey")
sp<-"Ortalis_vetula"
sp<-"Corvus_ossifragus"
sp<-"Falco_peregrinus"
sp_list<-c()
for (sp in names(sp_shape_list)){
  sp_shape<-sp_shape_list[[sp]]
  print(sp)
  #Presence, 1:Extant, 2:Probably Extant, 3:Possibly Extant, 4:Possibly Extinct, 
  #5:Extinct, 6:Presence Uncertain
  
  #Origin Codes, 1: Native, 2:Reintroduced, 3:Introduced, 4:Vagrant,
  #5:Origin Uncertain, 6:Assisted Colonisation
  
  #Seasonality Codes, 1:Resident, 2:Breeding Season, 3:Non-breeding Season,
  #4:Passage, 5:Seasonal Occurrence Uncertain
  sp_shape<-sp_shape[which(sp_shape$origin %in% c(1, 2)),]
  #plot(st_geometry(sp_shape), col="red", add=T)
  if (nrow(sp_shape)>0){
    if (length(unlist(st_intersects(sp_shape, na)))>0){
      sp_list<-c(sp_list, sp)
    }
  }
}
saveRDS(sp_list, "../../Urban/Tables/IUCN_checklist_in_NA_2021.rda")


if (F){
  london<-readRDS("/media/huijieqiao/WD12T/eBird/Tables/Locality_202105/United Kingdom/England/London/raw.rda")
  
  
  london<-london[which(london$YEAR==2015),]
  london$APPROVED
  london$REVIEWED
  dim(london)
  table(london[, c("APPROVED", "REVIEWED")])
  
}
length(sp_list)
dd<-readRDS("../../Urban/Tables/IUCN_checklist_in_NA_v2.rda")
length(dd)

if (F){
  iucn_2020_not_in_2021<-dd[!(dd %in% sp_list)]
  iucn_2021_not_in_2020<-dd[!(sp_list %in% dd)]
}

countries_in_north_america<-c("United States", "Mexico", "Canada", "Guatemala", "Cuba", "Haiti",
                              "Dominican Republic", "Honduras", "Nicaragua", "El Salvador", "Costa Rica", "Panama", "Jamaica",
                              "Trinidad and Tobago", "Belize", "Bahamas", "Barbados", "Saint Lucia", "Grenada",
                              "Saint Vincent and the Grenadines", "Antigua and Barbuda", "Dominica", "Saint Kitts and Nevis")

folder_t<-"/media/huijieqiao/WD12T/eBird/Tables/Locality_202105/%s"
#folder_t<-"X:/eBird/Tables/Locality_202105/%s"

country<-countries_in_north_america[1]
country<-"Cuba"
all_approved<-list()
all_reviewed<-list()
all_approved_and_reviewed<-list()

for (country in countries_in_north_america){
  dirs<-list.dirs(sprintf(folder_t, country))
  d<-dirs[3]
  if (country=="United States"){
    dirs<-dirs[which(!grepl("Hawaii", dirs))]  
  }
  
  for (d in dirs){
    print(d)
    file<-sprintf("%s/raw.rda", d)
    if (file.exists(file)){
      item<-data.table(readRDS(file))
      item<-item[between(YEAR, 2015, 2019)]
      ll_cols<-c("LONGITUDE", "LATITUDE")
      occ_points<-st_as_sf(item[, ..ll_cols], coords = ll_cols, crs=st_crs(mask))
      occ_points<-st_transform(occ_points, crs=st_crs(mask_10km))
      item<-item[st_contains(na, occ_points)[[1]]]
      if (nrow(item)==0){
        next()
      }
      
      item_approved<-item[APPROVED==1]
      if (nrow(item_approved)>0){
        item_approved_se<-item_approved[, .(N=.N), by=list(SCIENTIFIC_NAME, YEAR)]
        all_approved[[d]]<-item_approved_se
      }

      item_reviewed<-item[REVIEWED==1]
      if (nrow(item_reviewed)>0){
        item_reviewed_se<-item_reviewed[, .(N=.N), by=list(SCIENTIFIC_NAME, YEAR)]
        all_reviewed[[d]]<-item_reviewed_se
      }
      
      item_approved_and_reviewed<-item[(APPROVED==1)|(REVIEWED==1)]
      if (nrow(item_approved_and_reviewed)>0){
        item_approved_and_reviewed_se<-item_approved_and_reviewed[, .(N=.N), by=list(SCIENTIFIC_NAME, YEAR)]
        all_approved_and_reviewed[[d]]<-item_approved_and_reviewed_se
      }
      
    }
  }
}

all_approved<-rbindlist(all_approved)
all_reviewed<-rbindlist(all_reviewed)
all_approved_and_reviewed<-rbindlist(all_approved_and_reviewed)


all_approved_se<-all_approved[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, YEAR)]
saveRDS(all_approved_se, "../../Urban/Tables/eBird_checklist_in_NA_approved_2021.rda")

all_reviewed_se<-all_reviewed[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, YEAR)]
saveRDS(all_reviewed_se, "../../Urban/Tables/eBird_checklist_in_NA_reviewed_2021.rda")

all_approved_and_reviewed_se<-all_approved_and_reviewed[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, YEAR)]
saveRDS(all_approved_and_reviewed_se, "../../Urban/Tables/eBird_checklist_in_NA_approved_and_reviewed_2021.rda")



all_se<-readRDS("../../Urban/Tables/eBird_checklist_in_NA_v2.rda")
sp_list<-readRDS("../../Urban/Tables/IUCN_checklist_in_NA_v2.rda")
sp_list<-gsub("_", " ", sp_list)
all_IUCN<-names(sp_shape_list)
all_IUCN<-gsub("_", " ", all_IUCN)

all_se$in_IUCN<-0
all_se[SCIENTIFIC_NAME %in% all_IUCN]$in_IUCN<-1
all_se[SCIENTIFIC_NAME %in% sp_list]$in_IUCN<-2
all_se[grepl("/", SCIENTIFIC_NAME)]$in_IUCN<--1
all_se[grepl("sp\\.", SCIENTIFIC_NAME)]$in_IUCN<--1
all_se[grepl(" x ", SCIENTIFIC_NAME)]$in_IUCN<--1
all_se[grepl("\\(", SCIENTIFIC_NAME)]$in_IUCN<--1

table(all_se$in_IUCN)


other_sp<-sp_list[!(sp_list %in% all_se$SCIENTIFIC_NAME)]
sp_list_NB<-data.table(SCIENTIFIC_NAME=sp_list, in_eBird=F)
sp_list_NB[(SCIENTIFIC_NAME %in% all_se$SCIENTIFIC_NAME)]$in_eBird<-T
table(sp_list_NB$in_eBird)
saveRDS(all_se, "../../Urban/Tables/eBird_checklist_in_NA_v2.rda")
saveRDS(sp_list_NB, "../../Urban/Tables/IUCN_checklist_in_NA_with_eBird_v2.rda")

dim(readRDS("../../Urban/Tables/eBird_checklist_in_NA.rda"))
dim(all_se)
all_se<-readRDS("../../Urban/Tables/eBird_checklist_in_NA.rda")
sp_list<-unique(all_se$SCIENTIFIC_NAME)

sp<-sp_list[1]
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="eBird", host="172.16.120.11")
#SQL<-sprintf("SELECT DISTINCT COMMON_NAME FROM eBird202105 WHERE SCIENTIFIC_NAME='%s'", 
#             sp)
SQL<-sprintf("SELECT DISTINCT SCIENTIFIC_NAME FROM eBird202105")
rs<-dbSendQuery(con, SQL)
records<-fetch(rs, n=-1)
dbClearResult(rs)
dbDisconnect(con)

iucnlist<-names(sp_shape_list)
ebirdlist<-records$SCIENTIFIC_NAME
ebirdlist_removed<-ebirdlist[!grepl("/", ebirdlist)]
ebirdlist_removed<-ebirdlist[!grepl("sp\\.", ebirdlist)]
ebirdlist_removed<-ebirdlist[!grepl(" x ", ebirdlist)]
ebirdlist_removed<-ebirdlist[!grepl("\\(", ebirdlist)]


saveRDS(iucnlist, "../../Urban/Tables/iucnlist.rda")
saveRDS(ebirdlist_removed, "../../Urban/Tables/ebirdlist_removed.rda")
saveRDS(ebirdlist, "../../Urban/Tables/ebirdlist.rda")


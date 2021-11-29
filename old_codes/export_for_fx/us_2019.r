library("RMySQL")
library("data.table")
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")

i=1
SQL_template<-'SELECT CATEGORY, COMMON_NAME, \
SCIENTIFIC_NAME, \
OBSERVATION_DATE,\
OBSERVATION_COUNT, \
APPROVED, REVIEWED, STATE, COUNTY, LOCALITY, LOCALITY_TYPE, \
LATITUDE, LONGITUDE, TRIP_COMMENTS,  SPECIES_COMMENTS\
FROM eBird202105 WHERE COUNTRY="United States" and STATE="%s" and COUNTY="%s"'


folder<-"/media/huijieqiao/WD12T/eBird/Tables/Locality_202105/United States"
dirs<-list.dirs(folder, full.names = F)
d<-dirs[10]
all<-list()
if (F){
  all_p<-readRDS("../../Urban/US_Raw/us_2019.rda")
  all_p$label<-paste(all_p$STATE, all_p$COUNTY,  sep="/")
  for (n in unique(all_p$label)){
    print(n)
    item<-all_p[label==n]
    all[[n]]<-item
  }
}

for (d in dirs){
  d_item<-strsplit(d, "/")[[1]]
  if (length(d_item)==2){
    state<-d_item[1]
    county<-d_item[2]
    target<-sprintf("../../Urban/US_Raw/States/%s/%s.rda", state, county)
    if (file.exists(target)){
      next()
    }
    dir.create(sprintf("../../Urban/US_Raw/States/%s", state), showWarnings = F)
    saveRDS(NULL, target)
    print(d_item)
    con<-dbConnect(MySQL(), user="root", password="mikania", 
                   dbname="eBird", host="172.16.120.11")
    county_label<-ifelse(county=='Unnamed', '', county)
    SQL<-sprintf(SQL_template, state, county_label)
    rs<-dbSendQuery(con, SQL)
    records<-fetch(rs, n=-1)
    dbClearResult(rs)
    dbDisconnect(con)
    records$OBS_DATE<-as.Date(records$OBSERVATION_DATE, format="%Y-%m-%d")
    #records<-records%>%dplyr::filter(OBS_DATE>="2009-1-1")
    #records<-records%>%dplyr::filter(APPROVED==1)
    records$YEAR<-as.numeric(format(records$OBS_DATE,"%Y"))
    records$MONTH<-as.numeric(format(records$OBS_DATE,"%m"))
    records$DAY<-as.numeric(format(records$OBS_DATE,"%d"))
    all_item<-list()
    for (year in c(2010:2021)){
      records_item<-records[which(records$YEAR==year),] 
      all_item[[as.character(year)]]<-records_item
    }
    saveRDS(all_item, target)
  }
}
if (F){
  all<-list()
  for (year in c(2010:2021)){
    all[[as.character(year)]]<-list()
  }
  for (d in dirs){
    d_item<-strsplit(d, "/")[[1]]
    if (d %in% names(all[[as.character(2010)]])){
      next()
    }
    if (length(d_item)==2){
      state<-d_item[1]
      county<-d_item[2]
      print(d_item)
      target<-sprintf("../../Urban/US_Raw/States/%s/%s.rda", state, county)
      if (!file.exists(target)){
        next()
      }
      if (file.size(target)<=408){
        next()
      }
      item<-readRDS(target)
      for (year in c(2010:2021)){
        all_df<-item[[as.character(year)]]
        all[[as.character(year)]][[d]]<-all_df
      }
    }
  }
  for (year in c(2010:2021)){
    saveRDS(all[[as.character(year)]], sprintf("../../Urban/US_Raw/Years/%d.rda", year))
  }
  
}
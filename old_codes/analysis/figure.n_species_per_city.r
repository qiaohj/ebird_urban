library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
all_city_result<-readRDS("../../Urban/Tables/US_city_result.rda")
#all_city_result<-all_city_result[(!(grepl(", HI", CITY)))&(!(grepl(", AK", CITY)))]
all_city_result<-all_city_result[(!(grepl(", HI", CITY)))]

all_city_result$AREA<-all_city_result$CITY_AREA
all_city_result[TYPE!="CITY"]$AREA<-all_city_result[TYPE!="CITY"]$BUFFER_AREA-
  all_city_result[TYPE!="CITY"]$CITY_AREA

all_city_result$FIXED_SPECIES<-all_city_result$N_SPECIES/log(all_city_result$AREA)/log(all_city_result$N_EVENT)
city<-all_city_result[TYPE=="CITY"]
buffer<-all_city_result[TYPE=="BUFFER"]

all_city_result$FIXED<-all_city_result$N_SPECIES/all_city_result$CITY_AREA
all_city_result[TYPE=="BUFFER"]$FIXED<-
  all_city_result[TYPE=="BUFFER"]$N_SPECIES/
  (all_city_result[TYPE=="BUFFER"]$BUFFER_AREA-all_city_result[TYPE=="BUFFER"]$CITY_AREA)

ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")])+
  geom_point(aes(x=CENTROID_Y, y=FIXED, color=TYPE, shape=Native), alpha=0.2)+
  #geom_smooth(aes(x=CENTROID_Y, y=FIXED, color=TYPE, linetype=Native))+
  geom_smooth(method=lm, aes(x=CENTROID_Y, y=FIXED, color=TYPE, linetype=Native))+
  scale_y_log10()+
  theme_bw()

ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")])+
  geom_point(aes(x=CENTROID_Y, y=N_SPECIES, color=TYPE, shape=Native), alpha=0.2)+
  #geom_smooth(aes(x=CENTROID_Y, y=N_SPECIES, color=TYPE, linetype=Native))+
  geom_smooth(method=lm, aes(x=CENTROID_Y, y=N_SPECIES, color=TYPE, linetype=Native))+
  #scale_y_log10()+
  theme_bw()

ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")])+
  geom_point(aes(x=CENTROID_Y, y=FIXED_SPECIES, color=TYPE, shape=Native), alpha=0.2)+
  #geom_smooth(aes(x=CENTROID_Y, y=N_SPECIES, color=TYPE, linetype=Native))+
  geom_smooth(method=lm, aes(x=CENTROID_Y, y=FIXED_SPECIES, color=TYPE, linetype=Native))+
  #scale_y_log10()+
  theme_bw()

ggplot(all_city_result[(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])+
  geom_boxplot(aes(x=Native, y=N_SPECIES, color=TYPE))+
  facet_wrap(~YEAR, nrow=1)+theme_bw()

ggplot(all_city_result[(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])+
  geom_boxplot(aes(x=Native, y=N_EVENT, color=TYPE))+
  scale_y_log10()+
  facet_wrap(~YEAR, nrow=1)+theme_bw()


ggplot(all_city_result[(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])+
  geom_boxplot(aes(x=Native, y=N_EVENT, color=TYPE))+
  scale_y_log10()+
  facet_wrap(~YEAR, nrow=1)+theme_bw()


all_city_result$AREA<-all_city_result$CITY_AREA
all_city_result[TYPE!="CITY"]$AREA<-all_city_result[TYPE!="CITY"]$BUFFER_AREA-
  all_city_result[TYPE!="CITY"]$CITY_AREA

model<-glm(N_SPECIES~N_EVENT+AREA+TYPE, data=all_city_result[(Native==T)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])
summary(model)

model<-glm(N_SPECIES~N_EVENT+AREA+TYPE, data=all_city_result[(Native==F)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])
summary(model)


all_city_result$residuals<--9999
model<-glm(N_SPECIES~N_EVENT+AREA, data=all_city_result[(Native==F)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])
summary(model)
all_city_result[(Native==F)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")]$residuals<-model$residuals
model<-glm(N_SPECIES~N_EVENT+AREA, data=all_city_result[(Native==T)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")])
summary(model)
all_city_result[(Native==T)&(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")]$residuals<-model$residuals


range(all_city_result[(YEAR>=2015)&(YEAR<=2019)&(TYPE!="ALL")]$residuals)


ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")])+
  geom_boxplot(aes(x=Native, y=residuals, color=TYPE))+
  facet_wrap(~YEAR, nrow=1)+theme_bw()

ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")&(residuals>=-250)])+
  geom_point(aes(x=CENTROID_Y, y=residuals, color=TYPE))+
  geom_smooth(method = lm, aes(x=CENTROID_Y, y=residuals, color=TYPE))+theme_bw()+
  facet_wrap(~Native)

ggplot(all_city_result[(YEAR==2019)&(TYPE!="ALL")])+
  geom_point(aes(x=CENTROID_Y, y=N_SPECIES, color=TYPE))+
  geom_smooth(method = lm, aes(x=CENTROID_Y, y=residuals, color=TYPE))+theme_bw()+
  facet_wrap(~Native)

range(all_city_result$CENTROID_Y)
unique(all_city_result[CENTROID_Y<=2.5e6]$CITY)

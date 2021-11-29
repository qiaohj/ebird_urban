library(sf)
library(data.table)
setwd("X:/eBird/Script/ebird_migration")
continents<-st_read("../../Urban/Shape/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
countries_in_north_america<-c("United States", "Mexico", "Canada", "Guatemala", "Cuba", "Haiti",
                              "Dominican Republic", "Honduras", "Nicaragua", "El Salvador", "Costa Rica", "Panama", "Jamaica",
                              "Trinidad and Tobago", "Belize", "Bahamas", "Barbados", "Saint Lucia", "Grenada",
                              "Saint Vincent and the Grenadines", "Antigua and Barbuda", "Dominica", "Saint Kitts and Nevis")
countries<-continents[which(continents$CONTINENT=='North America'),]
plot(st_geometry(countries))
usa<-countries[which(countries$NAME_EN=='United States of America'),]
usa_cast <- st_cast(usa, "POLYGON")

cuba<-countries[which(countries$NAME_EN=='Cuba'),]
cuba_cast <- st_cast(cuba, "POLYGON")
cuba_cast$area<-st_area(cuba_cast)
plot(st_geometry(cuba_cast[which(cuba_cast$area==max(cuba_cast$area)),]))
threshold<-max(cuba_cast$area)
country<-'United States of America'
all_items<-NULL
countries<-st_cast(countries, "POLYGON")
countries$area<-st_area(countries)
#Belize
Belize<-countries[which(countries$NAME_EN=='Belize'),]
Belize_threshold<-max(Belize$area)
#El Salvador
Salvador<-countries[which(countries$NAME_EN=='El Salvador'),]
Salvador_threshold<-max(Salvador$area)
#Costa Rica
Costa_Rica<-countries[which(countries$NAME_EN=='Costa Rica'),]
Costa_Rica_threshold<-max(Costa_Rica$area)
#Panama
Panama<-countries[which(countries$NAME_EN=='Panama'),]
Panama_threshold<-max(Panama$area)


countries<-countries[which((countries$area>threshold)|
                       ((countries$NAME_EN=='Belize')&(countries$area==Belize_threshold))|
                       ((countries$NAME_EN=='El Salvador')&(countries$area==Salvador_threshold))|
                       ((countries$NAME_EN=='Costa Rica')&(countries$area==Costa_Rica_threshold))|
                       ((countries$NAME_EN=='Panama')&(countries$area==Panama_threshold))),]
range(countries$area)
plot(st_geometry(countries))
st_write(countries, "../../Urban/Shape/north_america_filtered/north_america_filtered.shp", append=FALSE)
#st_crs(continents)<-st_crs(mask)
#continents<-st_transform(continents, crs=st_crs(sp_shape_list[[1]]))
#mask<-raster("../../Urban/Raster/mask.tif")



north_america<-st_read("../../Urban/Shape/north_america_filtered_v2/north_america_filtered.shp")

north_america_un<-st_union(north_america)
plot(st_geometry(north_america_un))
st_write(north_america_un, "../../Urban/Shape/north_america_continent/north_america_continent.shp")

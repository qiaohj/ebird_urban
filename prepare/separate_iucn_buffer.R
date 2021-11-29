library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
sp_buffer_list<-readRDS("../../Urban/Shape/IUCN_Range_With_Buffer_2021/sp_buffer.rda")
sp<-names(sp_buffer_list)[1]
for (sp in names(sp_buffer_list)){
  print(sp)
  item<-sp_buffer_list[[sp]]
  saveRDS(item, sprintf("../../Shape/IUCN_Distribution_2021/buffer/%s.rda", sp))
}

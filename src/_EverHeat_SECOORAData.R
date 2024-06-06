## Aquatic Heatwave - Data Explore SECOORA
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2023-05-13


# https://github.com/r-spatial/rgee?tab=readme-ov-file
# library(rgeeExtra)
library(AnalystHelper)
library(reshape2)
library(plyr)

## Spatial
library(EVERSpatDat)
library(sf)
library(raster)

wd="C:/Julian_LaCie/_GitHub/EverHeat"
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))

#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

GIS.path.gen="C:/Julian_LaCie/_GISData"

# CRS
nad83.pro=st_crs("EPSG:4269")
utm17=st_crs("EPSG:26917")
wgs84=st_crs("EPSG:4326")



# GIS ---------------------------------------------------------------------
FLAB = paste0(GIS.path.gen,"/DOI/ENP")|>
  st_read("floridaBay_subbasins")|>
  st_transform(utm17)

datalist<-data(package = "EVERSpatDat"); # see a list of the dataset
data(list=datalist$results[,3]);# Loads all the data in the package

ENP <- subset(nps_clipped, UNIT_CODE=="EVER"); # Subsets for just ENP

# SECOORA -----------------------------------------------------------------
## (https://portal.secoora.org/#map)
library(rerddap)

data_meta=data.frame(tabledap("allDatasets",url="https://erddap.secoora.org/erddap/"))
data_meta=data_meta[data_meta$datasetID!="allDatasets",]

vars=c("datasetID","cdm_data_type","maxLongitude","maxLatitude")
data.shp=st_as_sf(data_meta[,vars],coords=c(vars[3:4]),crs=wgs84)|>
  st_transform(utm17)

# plot(st_geometry(ENP))
# plot(st_geometry(data.shp),add=T,pch=21,bg=adjustcolor("red",0.5))
# plot(st_geometry(data.shp[st_buffer(ENP,1000),]))

ENP.sites=data.shp[st_buffer(ENP,1000),]

out=ed_search(query="sea_water_temperature",which='table', url = "https://erddap.secoora.org/erddap/")
# out2=ed_search(query="sea_water_practical_salinity",which='table', url = "https://erddap.secoora.org/erddap/")
# out$info[out$info$dataset_id%in%out2$info$dataset_id,]

ENP.sites=subset(ENP.sites,datasetID%in%out$info$dataset_id)
plot(st_geometry(ENP.sites))

ENP.sites$datasetID

## check period of record, number of sample days, etc
# vars=c("time","WT")
# site.dur=data.frame()
# SECOORA.dat.ENP=data.frame()
# SECOORA.dat.da.ENP=data.frame()
# for(i in 1:nrow(ENP.sites)){
#   tmp.dat=tabledap(ENP.sites$datasetID[i],url="https://erddap.secoora.org/erddap/")|>
#     transform(datasetID=ENP.sites$datasetID[i],
#               Date=date.fun(time))|>
#     rename(c("sea_water_temperature"="WT"))|>
#     subset(is.na(WT)==FALSE)
#   
#   tmp.calc.da=ddply(tmp.dat,c("datasetID","Date"),summarise,N.val=N.obs(WT))
#   
#   tmp.calc=ddply(tmp.dat,c("datasetID"),summarise,minDate=min(Date),maxDate=max(Date),N.val=N.obs(WT))
#   tmp.calc$date.Delta=with(tmp.calc,as.numeric(maxDate-minDate)/365)
#   tmp.calc$N.datadays=nrow(tmp.calc.da)
#   tmp.calc$scrn=with(tmp.calc,ifelse(date.Delta>=10&N.datadays>=3000,1,0));# >=10 year which would be ~3000 sample days (365*10)
#   site.dur=rbind(site.dur,tmp.calc)
#   
#   if(tmp.calc$scrn==1){
#     SECOORA.dat.ENP=rbind(SECOORA.dat.ENP,tmp.dat[,vars])
#     SECOORA.dat.da.ENP=rbind(SECOORA.dat.da.ENP,tmp.calc.da)
#     }else{next}
#   print(i)
# }
# beepr::beep(4)

# write.csv(site.dur,paste0(export.path,"seacoora_ENP_inventory.csv"),row.names=F)
# write.csv(SECOORA.dat.ENP,paste0(export.path,"seacoora_ENP.csv"),row.names=F)
# write.csv(SECOORA.dat.da.ENP,paste0(export.path,"seacoora_ENP_daily.csv"),row.names=F)

site.dur=read.csv(paste0(export.path,"seacoora_ENP_inventory.csv"))
plot(sfwmd_bound)

plot(st_geometry(subset(ENP.sites,datasetID%in%subset(site.dur,scrn==1)$datasetID)))
     
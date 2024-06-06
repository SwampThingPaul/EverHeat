## Aquatic Heatwave
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

## FUNCTIONS
st_txt=function(x, y=NULL, labels,halo=T, col='black', hc='white', hw=0.1, ...){
  ## combination of code from raster::text(halo=T) and basf package
  
  if (missing(labels)) {
    labels <- 1
  }
  if (length(labels) != nrow(x)) {
    labels <- labels[1]
    if (is.character(labels)) {
      i <- which(labels == names(x))
      if (i == 0) {
        i <- 1
      }
    }
    labels <- x[[labels]]
    
  }
  xy <- sf::st_coordinates(sf::st_centroid(x))[,1:2, drop = FALSE]
  options(warn=-1)
  xy <- list(x = xy[,1, drop = TRUE], y = xy[,2, drop = TRUE])
  xo <- hw * graphics::strwidth('A')
  yo <- hw * graphics::strheight('A')
  
  if(halo==TRUE){
    theta <- seq(pi/4, 2*pi, length.out=8*hw*10)  
    for (i in theta){
      text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=hc, ... )
    }
    text(xy$x, xy$y, labels, col=col, ... )}else{
      text(xy$x, xy$y, labels, col=col, ... )
    }
  
}


# -------------------------------------------------------------------------
dates=date.fun(c("2000-01-01","2020-12-31"))

## CCORAL site (CRE) ... proof of workflow
dat=DBHYDRO_daily(dates[1],dates[2],"UO833")
head(dat)

library(heatwaveR)

dat$t=as.Date(dat$Date)
dat$temp=dat$Data.Value
detect_event(dat[,c("t","temp")])

ts <- ts2clm(dat[,c("t","temp")], climatologyPeriod = c("2002-07-25", "2020-12-31"))
mhw <- detect_event(ts)

quantile(dat$temp,probs=0.90,na.rm=T)

range(mhw$event$duration)
mhw$event
## not working
event_line(mhw, spread = 180, metric = "intensity_max", 
           start_date = "2002-07-25", end_date = "2020-12-31")
event_line(mhw, spread = 180, metric = "intensity_mean", 
           start_date = "2002-07-25", end_date = "2020-12-31")
event_line(mhw, spread = 180, metric = "intensity_var", 
           start_date = "2002-07-25", end_date = "2020-12-31")

lolli_plot(mhw, metric ="intensity_max")

## SECOORA (https://portal.secoora.org/#map)
## CSV
SECOORA_dat=function(SDate,EDate,SiteName,Param="sea_water_temperature"){
  # SiteName="gov_usgs_nwis_022908295"
  # Param="sea_water_temperature"
  # SDate=as.POSIXct("2024-05-30 01:00",tz="America/New_York")
  # EDate=as.POSIXct("2024-06-04 12:59",tz="America/New_York")
  
  base.link="https://erddap.secoora.org/erddap/tabledap/"
  SiteName.val=paste0(SiteName,".csv?")
  coln.vals=paste0(paste("time",Param,paste(Param,"qc_agg",sep="_"),sep="%2C"),"%2C")
  
  attr(EDate,"tzone")<-"UTC"
  attr(SDate,"tzone")<-"UTC"
  SDate.val=paste0("%3E%3D",format(SDate,"%Y-%m-%d"),"T",paste(format(SDate,"%H"),format(SDate,"%M"),format(SDate,"%S"),sep="%3A"),"Z")
  EDate.val=paste0("%3C%3D",format(EDate,"%Y-%m-%d"),"T",paste(format(EDate,"%H"),format(EDate,"%M"),format(EDate,"%S"),sep="%3A"),"Z")
  
  link=paste0(base.link,SiteName.val,coln.vals,"z&time",SDate.val,"&time",EDate.val)
  
  tmp=readLines(link)
  tmp=read.csv(text=tmp)
  return(tmp)
}
example=SECOORA_dat(as.POSIXct("2024-05-30 01:00",tz="America/New_York"),
            as.POSIXct("2024-06-04 12:59",tz="America/New_York"),
            "gov_usgs_nwis_022908295")
## need to find a way to query site we want!! 
# https://erddap.secoora.org/erddap/
library(rerddap)
dat.invent=ed_datasets(url="https://erddap.secoora.org/erddap/")|>data.frame()

subset(dat.invent,Dataset.ID=="gov_usgs_nwis_022908295")

tmp=data.frame(dat.invent[grep("long",dat.invent$Summary,ignore.case = T),])
head(tmp)

test=readLines(tmp$Background.Info[1])
test[grep("latitude",test,ignore.case = T)]

info("gov_usgs_nwis_022908295",url="https://erddap.secoora.org/erddap/")
tabledap("gov_usgs_nwis_022908295",url="https://erddap.secoora.org/erddap/")


GFDat=tabledap("gov-nps-ever-gbtf1",url="https://erddap.secoora.org/erddap/")|>data.frame()
head(GFDat)
GFDat2=subset(GFDat,is.na(sea_water_temperature)==F)
GFDat2=GFDat2[,c("time","sea_water_temperature")]
GFDat2$Date=date.fun(GFDat2$time)
GFDat2.da=ddply(GFDat2,"Date",summarise,mean.Temp=mean(sea_water_temperature,na.rm=T))

plot(mean.Temp~Date,GFDat2.da)


colnames(GFDat2.da)=c("t","temp")
GFDat2.da$t=as.Date(GFDat2.da$t)
ts <- ts2clm(GFDat2.da, climatologyPeriod = c("2000-01-01", "2024-06-05"))
mhw <- detect_event(ts)

range(mhw$event$duration)
mhw$event
tail(mhw$event)
event_line(mhw, spread = 180, metric = "intensity_max", 
           start_date = "2000-01-01", end_date = "2024-06-05")


lolli_plot(mhw, metric ="intensity_max")

plot(intensity_max~date_start,mhw$event)

plot(temp~t,mhw$climatology,type="l")
lines(thresh~t,mhw$climatology,col="green")
lines(seas~t,mhw$climatology,col="red")



WRDat=tabledap("gov-nps-ever-wrbf1",url="https://erddap.secoora.org/erddap/")|>data.frame()
head(WRDat)
WRDat2=subset(WRDat,is.na(sea_water_temperature)==F)
WRDat2=WRDat2[,c("time","sea_water_temperature")]
WRDat2$Date=date.fun(WRDat2$time)
WRDat2.da=ddply(WRDat2,"Date",summarise,mean.Temp=mean(sea_water_temperature,na.rm=T))

plot(mean.Temp~Date,WRDat2.da)


colnames(WRDat2.da)=c("t","temp")
WRDat2.da$t=as.Date(WRDat2.da$t)
ts <- ts2clm(WRDat2.da, climatologyPeriod = c("2000-01-01", "2024-06-05"))
mhw <- detect_event(ts)

range(mhw$event$duration)
mhw$event
tail(mhw$event)
event_line(mhw, spread = 180, metric = "intensity_max", 
           start_date = "2000-01-01", end_date = "2024-06-05")


lolli_plot(mhw, metric ="intensity_max")


#### 
GFdat=readLines("https://erddap.secoora.org/erddap/tabledap/gov-nps-ever-gbtf1.csv?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D2024-01-01T00%3A00%3A00Z&time%3C%3D2024-06-04T00%3A00%3A00Z")
GFdat=read.csv(text=GFdat)
"https://erddap.secoora.org/erddap/tabledap/gov-nps-ever-tbyf1.csv?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D2024-05-25T13%3A00%3A00Z&time%3C%3D2024-06-04T13%3A00%3A00Z"
"https://erddap.secoora.org/erddap/tabledap/gov_usgs_nwis_022908295.csv?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D2024-05-25T15%3A30%3A00Z&time%3C%3D2024-06-04T15%3A30%3A00Z"
link2="https://erddap.secoora.org/erddap/tabledap/gov_usgs_nwis_022908295.csv?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D2024-05-30T05%3A00%3A00Z&time%3C%3D2024-06-03T16%3A59%3A00Z"

tmp=as.POSIXct("2024-06-04 01:00:00",tz="America/New_York");tmp
tmp=as.POSIXct("2024-06-04 01:00:00",tz="EST");tmp

attr(tmp, "tzone")
attr(tmp, "tzone") <- "UTC"
tmp


  
plot(subset(GFdat,is.na(sea_water_temperature)==F)$sea_water_temperature)

## netcdf
# library(ncdf4)
# ### Download first?
# link="https://erddap.secoora.org/erddap/tabledap/gov-nps-ever-gbtf1.nc?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D2000-01-01T05%3A00%3A00Z&time%3C%3D2024-06-04T09%3A00%3A00Z"
# nc_data <- nc_open(link)

rm(list = ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(zoo)

setwd('C:/Users/karndt.WHRC/Desktop/sites/YKD')

#read in the full dataset
era = fread('./data/ERA5hourly_2019_2023_YKD.csv')
names(era)
#make more R friendly names
names(era)[c(1,17:30)] = c('index','date','dew','st1','st2','le','pres','h','rad','airt','ppt','u','v','vwc1','vwc2')

#subset down one site at a time
burn = subset(era,era$Site_Id == 'US-YK1')

#adjust to the timezone of interest (make sure it's listed in UTC)
burn$date
tz = -9 #hours from UTC
burn$date = burn$date+(tz*60*60)

#subset down to time range of interest
burn = subset(burn,burn$date >= as.POSIXct('2017-1-1',tz='UTC'))

#convert temps from K to deg C
burn$airt  = burn$airt-273.15
burn$dew   = burn$dew-273.15
burn$st1 = burn$st1-273.15
burn$st2 = burn$st2-273.15

#make negatives NAs so we can fill them using linear interpolation.
burn$rad = burn$rad/3600 #convert from J m-2 to Wm-2, divide by seconds in an hour

#caluclate rh from the dewpoint and temperature
burn$rh = 100*(exp((17.625*burn$dew)/(243.04+burn$dew))/exp((17.625*burn$airt)/(243.04+burn$airt)))

#create windspeed from u and v
burn$ws = sqrt(burn$v^2 + burn$u^2)

#create a date data frame with every half hour in the timeframe of interest
date = seq(from = as.POSIXct('2017-1-1 00:00',tz='UTC'),
           to = as.POSIXct('2023-1-1 00:00',tz='UTC'),
           by = 60*30)
datedf = as.data.frame(date)

#merge with era 5
burnm = merge(datedf,burn,by = 'date',all = T)

#gapfill middle half hours
burnm$dew   = na.approx(object = burnm$dew,maxgap = 6)
burnm$rh   = na.approx(object = burnm$rh,maxgap = 6)
burnm$st1   = na.approx(object = burnm$st2,maxgap = 6)
burnm$st2   = na.approx(object = burnm$st2,maxgap = 6)
burnm$rad   = na.approx(object = burnm$rad,maxgap = 6)
burnm$ppt   = na.approx(object = burnm$ppt,maxgap = 6)
burnm$pres   = na.approx(object = burnm$pres,maxgap = 6)
burnm$airt  = na.approx(object = burnm$airt,maxgap = 6)
burnm$vwc1   = na.approx(object = burnm$vwc1,maxgap = 6)
burnm$vwc2   = na.approx(object = burnm$vwc2,maxgap = 6)
burnm$ws   = na.approx(object = burnm$ws,maxgap = 6)


#check out the data
ggplot(data = burnm)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,airt,col='airT'))+
  geom_point(aes(date,st1,col='soilT1'))+
  geom_point(aes(date,st2,col='soilT2'))

ggplot(data = burnm)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rad))

ggplot(data = burnm)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,vwc1,col='vwc1'))+
  geom_point(aes(date,vwc2,col='vwc2'))
  
ggplot(data = burnm)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rh))

ggplot(data = burnm)+theme_bw()+
  geom_point(aes(date,pres))

ggplot(data = burnm)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,ws))

#resave off for comparison
write.csv(x = burnm,file = './data/era5_burn.csv',row.names = F)


#subset down one site at a time
unbu = subset(era,era$Site_Id == 'US-YK2')

#adjust to the timezone of interest (make sure it's listed in UTC)
unbu$date
tz = -9 #hours from UTC
unbu$date = unbu$date+(tz*60*60)

#subset down to time range of interest
#unbu = subset(unbu,unbu$date >= as.POSIXct('2017-1-1',tz='UTC'))

#convert temps from K to deg C
unbu$airt  = unbu$airt-273.15
unbu$dew   = unbu$dew-273.15
unbu$st1 = unbu$st1-273.15
unbu$st2 = unbu$st2-273.15

#make negatives NAs so we can fill them using linear interpolation.
unbu$rad = unbu$rad/3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
unbu$le = unbu$le/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
unbu$h = unbu$h/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour

#caluclate rh from the dewpoint and temperature
unbu$rh = 100*(exp((17.625*unbu$dew)/(243.04+unbu$dew))/exp((17.625*unbu$airt)/(243.04+unbu$airt)))

#create windspeed from u and v
unbu$ws = sqrt(unbu$v^2 + unbu$u^2)

#create a date data frame with every half hour in the timeframe of interest
date = seq(from = as.POSIXct('2019-1-1 00:00',tz='UTC'),
           to = as.POSIXct('2023-12-31 23:30',tz='UTC'),
           by = 60*30)
datedf = as.data.frame(date)

#merge with era 5
unbum = merge(datedf,unbu,by = 'date',all = T)

#gapfill middle half hours
unbum$dew   = na.approx(object = unbum$dew,maxgap = 6)
unbum$rh    = na.approx(object = unbum$rh,maxgap = 6)
unbum$st1   = na.approx(object = unbum$st2,maxgap = 6)
unbum$st2   = na.approx(object = unbum$st2,maxgap = 6)
unbum$rad   = na.approx(object = unbum$rad,maxgap = 6)
unbum$le    = na.approx(object = unbum$le,maxgap = 6)
unbum$h     = na.approx(object = unbum$h,maxgap = 6)
unbum$ppt   = na.approx(object = unbum$ppt,maxgap = 6)
unbum$pres  = na.approx(object = unbum$pres,maxgap = 6)
unbum$airt  = na.approx(object = unbum$airt,maxgap = 6)
unbum$vwc1  = na.approx(object = unbum$vwc1,maxgap = 6)
unbum$vwc2  = na.approx(object = unbum$vwc2,maxgap = 6)
unbum$ws    = na.approx(object = unbum$ws,maxgap = 6)

#check out the data
ggplot(data = unbum)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,airt,col='airT'))+
  geom_point(aes(date,st1,col='soilT1'))+
  geom_point(aes(date,st2,col='soilT2'))

ggplot(data = unbum)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rad))

ggplot(data = unbum)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,vwc1,col='vwc1'))+
  geom_point(aes(date,vwc2,col='vwc2'))

ggplot(data = unbum)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rh))

ggplot(data = unbum)+theme_bw()+
  geom_point(aes(date,pres))

ggplot(data = unbum)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,ws))

#resave off for comparison
write.csv(x = unbum,file = './data/era5_ykd_unburned.csv',row.names = F)
write.csv(x = unbum,file = './Ameriflux YK/unburned/era5_ykd_unburned.csv',row.names = F)

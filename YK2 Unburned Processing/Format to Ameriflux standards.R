rm(list = ls())

library(data.table)
library(ggplot2)
library(zoo)
Sys.setenv(TZ = "UTC")

setwd('C:/Users/karndt.WHRC/Desktop/sites/YKD/data/unburned/')

########## 2023 ####################
c23 = fread(input = "./YKD_UB_2023_clean.csv")
c23$ts = as.POSIXct(c23$ts,format = '%m/%d/%Y %H:%M')

fullts = seq(from = as.POSIXct('2023-01-01 00:00'), to = as.POSIXct('2023-12-31 23:30'),by = 60*30)
fullts = as.data.frame(fullts)
names(fullts) = 'ts'

c23 = merge(fullts,c23,all = T)

ggplot(data = c23)+
  geom_point(aes(ts,co2_flux.c))

ggplot(data = c23)+
  geom_point(aes(ts,ch4_flux.c))

ggplot(data = c23)+
  geom_point(aes(ts,H.c))

ggplot(data = c23)+
  geom_point(aes(ts,LE.c))

names(c23)
c23$ALB_1_1_1.c = ifelse(c23$ALB_1_1_1 < 0 | c23$ALB_1_1_1 > 1, NA, c23$ALB_1_1_1)
ggplot(data = c23)+
  geom_point(aes(ts,ALB_1_1_1.c))

ggplot(data = c23)+
  geom_point(aes(ts,LWIN_1_1_1,col='LWIN_1_1_1'))+
  geom_point(aes(ts,LWOUT_1_1_1,col='LWOUT_1_1_1'))
  
c23$LWIN_1_1_1.c = c23$LWIN_1_1_1
c23$LWOUT_1_1_1.c = c23$LWOUT_1_1_1

ggplot(data = c23)+
  geom_point(aes(ts,PPFD_1_1_1,col='PPFD_1_1_1'))+
  geom_point(aes(ts,SWIN_1_1_1,col='SWIN_1_1_1'))+
  geom_point(aes(ts,SWOUT_1_1_1,col='SWOUT_1_1_1'))

c23$PPFD_1_1_1.c = c23$PPFD_1_1_1
c23$SWIN_1_1_1.c = c23$SWIN_1_1_1
c23$SWOUT_1_1_1.c = c23$SWOUT_1_1_1

ggplot(data = c23)+
  geom_point(aes(ts,SHF_1_1_1,col='SHF_1_1_1'))+
  geom_point(aes(ts,SHF_2_1_1,col='SHF_2_1_1'))+
  geom_point(aes(ts,SHF_3_1_1,col='SHF_3_1_1'))

c23$SHF_1_1_1.c = c23$SHF_1_1_1
c23$SHF_2_1_1.c = c23$SHF_2_1_1
c23$SHF_3_1_1.c = c23$SHF_3_1_1

ggplot(data = c23)+
  geom_point(aes(ts,SWC_1_1_1,col='SWC_1_1_1'))+
  geom_point(aes(ts,SWC_2_1_1,col='SWC_2_1_1'))+
  geom_point(aes(ts,SWC_3_1_1,col='SWC_3_1_1'))

c23$SWC_1_1_1.c = c23$SWC_1_1_1
c23$SWC_2_1_1.c = c23$SWC_2_1_1
c23$SWC_3_1_1.c = c23$SWC_3_1_1

ggplot(data = c23)+
  geom_point(aes(ts,TS_1_1_1,col='TS_1_1_1'))+
  geom_point(aes(ts,TS_2_1_1,col='TS_2_1_1'))+
  geom_point(aes(ts,TS_3_1_1,col='TS_3_1_1'))

c23$TS_1_1_1.c = c23$TS_1_1_1
c23$TS_2_1_1.c = c23$TS_2_1_1
c23$TS_3_1_1.c = c23$TS_3_1_1

c23$TA_1_1_1.c = ifelse(c23$TA_1_1_1 > c23$air_t_mean + 5 | c23$TA_1_1_1 < c23$air_t_mean - 5,NA,c23$TA_1_1_1)
  
ggplot(data = c23)+geom_hline(yintercept = 0)+
  geom_point(aes(ts,TA_1_1_1.c-273.15))

ggplot(data = c23)+
  geom_point(aes(air_t_mean-273.15,TA_1_1_1.c-273.15))+
  geom_abline(intercept = 0,slope = 1,col='red')

c23$RH_1_1_1.c = ifelse(is.na(c23$TA_1_1_1.c),NA,c23$RH_1_1_1)

ggplot(data = c23)+geom_hline(yintercept = 0)+
  geom_point(aes(ts,RH_1_1_1.c))


c23$date = as.POSIXct(c23$date,format = '%m/%d/%Y')
c23$date.y = as.POSIXct(c23$date.y,format = '%m/%d/%Y')

write.csv(x = c23,file = './YKDUnburned_Cleaned_Met_Flux_2023_Ameriflux.csv',row.names = F)


################# 2024 ############################
c24 = fread("./YKD_UB_2024_clean.csv")

fullts = seq(from = as.POSIXct('2024-01-01 00:00'), to = as.POSIXct('2024-08-31 23:30'),by = 60*30)
fullts = as.data.frame(fullts)
names(fullts) = 'ts'

c24 = merge(fullts,c24,all = T)

ggplot(data = c24)+
  geom_point(aes(ts,co2_flux.c))

ggplot(data = c24)+
  geom_point(aes(ts,ch4_flux.c))

ggplot(data = c24)+
  geom_point(aes(ts,H.c))

ggplot(data = c24)+
  geom_point(aes(ts,LE.c))


c24$ALB_1_1_1.c = ifelse(c24$ALB_1_1_1 < 0 | c24$ALB_1_1_1 > 1, NA, c24$ALB_1_1_1)
ggplot(data = c24)+
  geom_point(aes(ts,ALB_1_1_1.c))

ggplot(data = c24)+
  geom_point(aes(ts,LWIN_1_1_1,col='LWIN_1_1_1'))+
  geom_point(aes(ts,LWOUT_1_1_1,col='LWOUT_1_1_1'))

c24$LWIN_1_1_1.c = c24$LWIN_1_1_1
c24$LWOUT_1_1_1.c = c24$LWOUT_1_1_1

ggplot(data = c24)+
  geom_point(aes(ts,PPFD_1_1_1,col='PPFD_1_1_1'))+
  geom_point(aes(ts,SWIN_1_1_1,col='SWIN_1_1_1'))+
  geom_point(aes(ts,SWOUT_1_1_1,col='SWOUT_1_1_1'))

c24$PPFD_1_1_1.c = c24$PPFD_1_1_1
c24$SWIN_1_1_1.c = c24$SWIN_1_1_1
c24$SWOUT_1_1_1.c = c24$SWOUT_1_1_1

ggplot(data = c24)+
  geom_point(aes(ts,SHF_1_1_1,col='SHF_1_1_1'))+
  geom_point(aes(ts,SHF_2_1_1,col='SHF_2_1_1'))+
  geom_point(aes(ts,SHF_3_1_1,col='SHF_3_1_1'))

c24$SHF_1_1_1.c = c24$SHF_1_1_1
c24$SHF_2_1_1.c = c24$SHF_2_1_1
c24$SHF_3_1_1.c = c24$SHF_3_1_1

ggplot(data = c24)+
  geom_point(aes(ts,SWC_1_1_1,col='SWC_1_1_1'))+
  geom_point(aes(ts,SWC_2_1_1,col='SWC_2_1_1'))+
  geom_point(aes(ts,SWC_3_1_1,col='SWC_3_1_1'))

c24$SWC_1_1_1.c = c24$SWC_1_1_1
c24$SWC_2_1_1.c = c24$SWC_2_1_1
c24$SWC_3_1_1.c = c24$SWC_3_1_1

ggplot(data = c24)+
  geom_point(aes(ts,TS_1_1_1,col='TS_1_1_1'))+
  geom_point(aes(ts,TS_2_1_1,col='TS_2_1_1'))+
  geom_point(aes(ts,TS_3_1_1,col='TS_3_1_1'))

c24$TS_1_1_1.c = c24$TS_1_1_1
c24$TS_2_1_1.c = c24$TS_2_1_1
c24$TS_3_1_1.c = c24$TS_3_1_1

c24$TA_1_1_1.c = ifelse(c24$TA_1_1_1 > c24$air_t_mean + 5 | c24$TA_1_1_1 < c24$air_t_mean - 5,NA,c24$TA_1_1_1)

ggplot(data = c24)+geom_hline(yintercept = 0)+
  geom_point(aes(ts,TA_1_1_1.c-273.15))

ggplot(data = c24)+
  geom_point(aes(air_t_mean-273.15,TA_1_1_1.c-273.15))+
  geom_abline(intercept = 0,slope = 1,col='red')

c24$RH_1_1_1.c = ifelse(is.na(c24$TA_1_1_1.c),NA,c24$RH_1_1_1)

ggplot(data = c24)+
  geom_point(aes(ts,RH_1_1_1.c))


c24$date = as.POSIXct(c24$date,format = '%m/%d/%Y')
c24$date.y = as.POSIXct(c24$date.y,format = '%m/%d/%Y')

write.csv(x = c24,file = './YKDUnburned_Cleaned_Met_Flux_2024_Ameriflux.csv',row.names = F)

rm(list = ls())
Sys.setenv(TZ = 'UTC')
setwd('C:/Users/karndt.WHRC/Desktop/sites/churchill/ameriflux submission')

library(data.table)
library(ggplot2)

#read the ungapfilled data
dat = fread('./churchill_2022_2023_gf.csv')

#read in the gapfilling output
gfh  = fread('./data/cf3-Results_data.txt',header = T,nrows = 0)
gf   = fread('./data/cf3-Results_data.txt',header = F,skip  = 2,na.strings = c('-9999','-10000','-9.9990e+03'))
names(gf) = names(gfh)

#break apart the time column
hour = floor(x = gf$Hour)
dif = gf$Hour - hour
min = dif*60
time = paste(hour,min,sep = ':')
date = paste(gf$Year,gf$DoY,sep = '-')
ts   = as.POSIXct(paste(date,time,sep = ' '),format = '%Y-%j %H:%M',tz='GMT')
gf$ts = ts

#reduce the gap filled data set
gfdat = data.frame(gf$ts,
                   gf$Rg_f,
#                   gf$H_f,
#                   gf$LE_f,
#                   gf$NEE_f,
#                   gf$NEE_fall,
#                   gf$CH4_f,
#                   gf$CH4_fall,
                   gf$Tsoil_f,
                   gf$Tair_f,
                   gf$VPD_f,
                   gf$RH_f,
                   gf$Reco,
                   gf$Reco_DT,
                   gf$GPP_f,
                   gf$GPP_DT)

names(gfdat) = c('ts',
                 'Rg_f',
#                 'H_f',
#                 'LE_f',
#                 'NEE_f',
#                 'NEE_fall',
#                 'CH4_f',
#                 'CH4_fall',
                 'Tsoil_f',
                 'Tair_f',
                 'VPD_f',
                 'RH_f',
                 'Reco',
                 'GPP_DT',
                 'GPP_f',
                 'Reco_DT')

#merge data frames
dat = merge(dat,gfdat,by = 'ts')
dat$dup = duplicated(x = dat$ts)
dat = subset(dat,dat$dup == 'FALSE')

summary(dat$ts)

write.csv(x = dat,file = './data/cf3_gapfilled_clean_2022_2023.csv',quote = F,row.names = F)


rm(list = ls())

library(data.table)
library(plyr)
library(ggplot2)
library(cowplot)
library(openair)
library(viridis)
library(dplyr)
library(lubridate)

#load in the full output flux data ##################
fp = 'C:/Users/dtrangmoe/Documents/Churchill/EP_full_output'
files = list.files(path = fp,pattern = '*full_output.+csv$',recursive = T,full.names = T)

#load the headers and data into their own lists
h   = lapply(files, fread,skip = 1,nrow = 0)
dat = lapply(files, fread,skip = 3,header = F,na.strings=c('-9999'))

ls()

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#make all the lists into one dataframe
df = dat[[1]]
for (i in 2:length(dat)) {
  df = rbind.fill(df,dat[[i]])
}

#create a timestamp from the date and time
df$ts = as.POSIXct(x = paste(df$date,df$time,sep = ' '),tz = 'UTC')
df = df[!duplicated(df$ts),]



## Replace Nov/Dec data with merged ch4/co2 files (if applicable) 
df_nov=fread('C:/Users/dtrangmoe/Documents/Churchill/ChurchillFlux_Nov23_CO2_CH4_merged.csv')
df_dec=fread('C:/Users/dtrangmoe/Documents/Churchill/ChurchillFlux_Dec23_CO2_CH4_merged.csv')

#Used to add in merged files 
df_subset <- subset(df, df$ts < as.POSIXct('2023-11-01 0030'))

# Combine df_subset, df_nov, and df_dec
df <- bind_rows(df_subset, df_nov, df_dec)




#take the min and max date rounding to the nearest full month
mindate = floor_date(min(df$ts),unit = 'month')
maxdate = ceiling_date(max(df$ts),unit = 'month')

#create a timestamp variable every half hour to the full months
ts = seq(from = mindate,to = maxdate,by = 60*30)
ts = as.data.frame(ts)


#merge with the flux data frame to create NAs where data is missing
df = merge(ts,df,by = 'ts',all.x = T)


#save off flux data
write.csv(df,'C:/Users/dtrangmoe/Documents/Churchill/Churchill_fluxes_merged.csv',row.names = F)









``````````````



fp = 'C:/Permafrost Pathways/eddypro/EP_biomet'
files = list.files(path = fp,pattern = '*biomet',recursive = T,full.names = T)

#load the headers and data into their own lists

h   = lapply(files, fread,skip = 0,nrow = 0)
dat = lapply(files, fread,skip = 2,header = F,na.strings=c('-9999'))

#assign the headers to the data

for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#make all the lists into one dataframe

met = dat[[1]]
for (i in 2:length(dat)) {
  met = rbind.fill(met,dat[[i]])
}

#create a timestamp from the date and time

met$ts = as.POSIXct(x = paste(met$date,met$time,sep = ' '),tz = 'UTC')

#merge datasets

#df= merge(met,df,by='ts',all = T)






















































co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-75,100),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
 # scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
  
ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,2),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
 # scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))










df$Time= df$ts



co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,co2_flux),size = .15)+
  scale_y_continuous(limits = c(-10,6),
                     expression(bold('NEE ('*mu*mol~CO[2]~m^-2*s^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2022-8-1','2023-2-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0),
        text = element_text(size = 8),
        axis.text = element_text(size = 8),
        panel.background = element_rect(fill = 'transparent'))
co2

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,ch4_flux*43.2),size = .15)+
  scale_y_continuous(limits = c(-1,8),
                     expression(bold(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2022-8-1','2023-2-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7),
        text = element_text(size = 8),
        axis.text = element_text(size = 8),
        panel.background = element_rect(fill = 'transparent'))
ch4


png(filename = 'C:/Permafrost Pathways/reallyfrigginmintdewd.png',width = 4,height = 3,units = 'in',bg = 'transparent',res = 1500)
plot_grid(co2,ch4,nrow = 2, align = "v")
dev.off()


















df$Time= df$ts

#plot co2 with qc colors
co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,co2_flux))+
  scale_y_continuous(limits = c(-12,12),
                     expression(bold('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2022-8-1','2023-2-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
co2

#plot co2 with qc=2 removed
co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-40,20),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2022-7-28','2023-2-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
co2

#plot ch4 with qc colors
ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,ch4_flux*43.2))+
  scale_y_continuous(limits = c(-1,9),
                     expression(bold(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2022-8-1','2023-2-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4


#plot ch4 with qc=2 removed

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,9),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2022-9-30','2022-10-28')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4

#plot H with colored Temp

H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(ts,H,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
H

#plot H with qc=2 removed
H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_H<2,],aes(ts,H,color=qc_H))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
H

#plot LE with colored Temp
LE = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_LE<2,],aes(ts,LE,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-20,75),
                     expression('LE (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
LE


#plot LE with qc=2 removed
LE = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_LE<2,],aes(ts,LE,color=qc_LE))+
  scale_y_continuous(limits = c(-20,350),
                     expression('LE (W/m^2 )'))+
  scale_x_datetime('',expand = c(.01,.01))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
LE

##################


LE = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_LE<2,],aes(ts,LE,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-20,50),
                     expression('LE (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))

H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_H<2,],aes(ts,H,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,7),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2022-8-8','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4


co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-60,150),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2022-10-28','2022-11-15')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))

plot_grid(co2,ch4, nrow = 2)

plot(df$qc_co2_flux,df$qc_H)
plot(df$ts,df$qc_LE)
plot(df$ts,df$qc_H)
plot(df$ts,df$qc_co2_flux)
plot(df$ts,df$qc_ch4_flux)
plot(df$ts,df$LE);abline(h=0,col='red')
plot(df$ts,df$H);abline(h=0,col='red')
plot(df$ts,df$co2_mixing_ratio);abline(h=0,col='red')
plot(df$ts,df$ch4_mixing_ratio);abline(h=0,col='red')
plot(df$ts,df$`u*`);abline(h=0,col='red')
plot(df$ts,df$wind_speed);abline(h=0,col='red')unle
plot(df$ts,df$wind_dir);abline(h=0,col='red')

windRose(mydata = df,ws = 'wind_speed',wd = 'wind_dir')





#create a timestamp from the date and time
df$ts = as.POSIXct(x = paste(df$date,df$time,sep = ' '),tz = 'UTC')



#load in the micromet data ################## ################################

#load in the first micromet file
h55   = read.csv('./met/h55/header55.csv')
met55 = read.csv('./met/h55/ChurchillMet_Micromet.dat',header = F,skip = 4,na.strings=c('-9999','NA','NAN'))

names(met55) = names(h55)

#load in the first micromet file
h81   = read.csv('./met/h81/header81.csv')
met81 = read.csv('./met/h81/ChurchillMetEthernet_Micromet_until20221012.dat',header = F,skip = 4,na.strings=c('-9999','NA','NAN'))

names(met81) = names(h81)

#load in the full output met data ##################
fp = './met/h79/'
files = list.files(path = fp,pattern = '*Micro',full.names = T)



#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}


ls()

#make all the lists into one dataframe
met = dat[[1]]
for (i in 2:length(dat)) {
  met=rbind.fill(met,dat[[i]])
}

met$ts = as.POSIXct(x=paste(met$date,sep = 'UTC'))

###########################################BIOMET FILES#####################################################

#load in the Biomet data ##################
fp = 'C:/Permafrost Pathways/eddypro/EP_biomet'
files = list.files(path = fp,pattern = '*biomet',recursive = T,full.names = T)

#load the headers and data into their own lists
h   = lapply(files, fread,skip = 0,nrow = 0)
dat = lapply(files, fread,skip = 2,header = F,na.strings=c('-9999'))

ls()



#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#make all the lists into one dataframe
met = dat[[1]]
for (i in 2:length(dat)) {
  met = rbind.fill(met,dat[[i]])
}

names(met)


met$ts = as.POSIXct(x=paste(met$date,sep = 'UTC'))

plot(met$ts,met$ThermocoupleT_Avg_1,ylim = c(-2,25));abline(h=0)
points(met$ts,met$ThermocoupleT_Avg_2,col='red')
points(met$ts,met$ThermocoupleT_Avg_3,col='orange')
points(met$ts,met$ThermocoupleT_Avg_4,col='yellow')
points(met$ts,met$ThermocoupleT_Avg_5,col='green')
points(met$ts,met$ThermocoupleT_Avg_6,col='blue')
points(met$ts,met$ThermocoupleT_Avg_7,col='cyan')
points(met$ts,met$ThermocoupleT_Avg_8,col='purple')
points(met$ts,met$ThermocoupleT_Avg_9,col='pink')
points(met$ts,met$ThermocoupleT_Avg_10)
points(met$ts,met$ThermocoupleT_Avg_11,col='red')
points(met$ts,met$ThermocoupleT_Avg_12,col='orange')

plot(met$ts,met$ThermocoupleT_Avg_13,ylim = c(-2,25));abline(h=0)
points(met$ts,met$ThermocoupleT_Avg_14,col='red')
points(met$ts,met$ThermocoupleT_Avg_15,col='orange')
points(met$ts,met$ThermocoupleT_Avg_16,col='yellow')
points(met$ts,met$ThermocoupleT_Avg_17,col='green')
points(met$ts,met$ThermocoupleT_Avg_18,col='blue')
points(met$ts,met$ThermocoupleT_Avg_19,col='cyan')
points(met$ts,met$ThermocoupleT_Avg_20,col='purple')
points(met$ts,met$ThermocoupleT_Avg_21,col='pink')
points(met$ts,met$ThermocoupleT_Avg_22)
points(met$ts,met$ThermocoupleT_Avg_23,col='red')
points(met$ts,met$ThermocoupleT_Avg_24,col='orange')


#####BIOMET PLOTS#################


#create a timestamp from the date and time of met

met$ts = as.POSIXct(x = paste(met$date,met$time,sep = ' '),tz = 'UTC')



ggplot(data = met)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=met,aes(ts,TA_1_1_1-273.15,color=TA_1_1_1-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2022-9-29','2022-10-25')))+
  scale_y_continuous(limits = c(-30,30))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))




ggplot(data = met)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_line(data=met,aes(ts,PPFD_1_1_1,color='Par'))+
  geom_line(data=met,aes(ts,PPFDR_1_1_1,color='Par Reflected')) +
  scale_x_datetime(limits = as.POSIXct(c('2022-11-3','2022-11-15')))+
  scale_y_continuous(limits = c(0,500))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))





ggplot(data = df)+theme_bw()+geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(ch4_flux.x*43.2,ch4_flux.y*43.2))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))+
  scale_x_continuous(limits=c(-2,7.5))+
  scale_y_continuous(limits=c(-2,7.5))

#other test plots
plot(met$ts,met$PanelT_Avg)
plot(met$ts,met$AirT_Avg)
plot(met$ts,met$RH)
plot(met$ts,met$WS_Avg)
plot(met$ts,met$WS_Max)
plot(met$ts,met$WD)
plot(met$ts,met$BP_Avg)
plot(met$ts,met$WD)
plot(met$ts,met$Precip_Tot)
plot(met$ts,met$SHF_Avg_1)
plot(met$ts,met$SHF_Avg_2)
plot(met$ts,met$DT_Avg)
plot(met$ts,met$Q_Avg)
plot(met$ts,met$SD_1_1_1,xlim = as.POSIXct(c('2022-10-28','2022-11-15')),ylim= c(0,.5))
plot(met$ts,met$PPFD_1_1_1)
points(met$ts,met$PPFDR_1_1_1, col='red')
plot(met$ts,met$PAR_Tot_in_Tot,ylim = c(-10,4000))
plot(met$ts,met$PAR_Tot_out_Tot)
plot(met$ts,met$PAR_Den_out_Avg)
plot(met$ts,met$RN_Avg)
plot(met$ts,met$RN_corr_Avg)
plot(met$ts,met$SW_in_Avg)
plot(met$ts,met$LW_in_Avg)

ggplot(data = met)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  


#all the data merged
m = merge(df,met,by='ts',all=T)

names(m)

m$g = (m$SHF_Avg_1+m$SHF_Avg_1)/2

ggplot(data = m,aes(RN_Avg-g,LE+H))+theme_bw()+geom_abline(slope = 1,intercept = 0)+
  geom_point()+
  geom_smooth(method = 'lm')

summary(lm(LE+H~RN_Avg-g,data = m))
 


#load in the full output biomet data ##################
fp = './eddypro/'
files = list.files(path = fp,pattern = '*biomet',recursive = T,full.names = T)

#load the headers and data into their own lists
h   = lapply(files, fread,nrow = 0)
dat = lapply(files, fread,skip = 2,header = F,na.strings=c('-9999'))

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#make all the lists into one dataframe
bm = dat[[1]]
for (i in 2:length(dat)) {
  bm = rbind.fill(bm,dat[[i]])
}

bm$ts = as.POSIXct(paste(bm$date,bm$time,sep=' '),tz = 'UTC')
plot(bm$ts,bm$ALB_1_1_1)
plot(bm$ts,bm$SWIN_1_1_1)
plot(bm$ts,bm$SWOUT_1_1_1)
plot(bm$ts,bm$LWIN_1_1_1)
plot(bm$ts,bm$LWOUT_1_1_1)


mer
plot(mer$H)
summary(mer)
sub = subset(mer,mer$ts > as.POSIXct('2022-9-25'))

ggplot(data = sub,aes(RN_1_1_1 - SHF_1_1_1,LE + H))+
  geom_point()


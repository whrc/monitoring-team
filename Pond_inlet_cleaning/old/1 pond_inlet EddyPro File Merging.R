

rm(list = ls())

library(data.table)
library(plyr)
library(ggplot2)
library(cowplot)
library(openair)
library(viridis)
library(dplyr)
library(lubridate)

####BELOW LINES IS FOR MERGING MULTIPLE MONTHS/FILES OF DATA####
####FOR SINGLE MONTH FILE PREPARATION SEE 2ND CHAPTHER @LINE 65
####################################################################

#load in the full output flux data ##################
fp = 'C:/Users/klynoe/Documents/pond_inlet/eddypro/full_output/2024'
#fp = 'C:/Users/klynoe/Documents/pond_inlet/202410_full/eddypro/full_output/2024'
files = list.files(path = fp,pattern = '*full_output.+csv$',recursive = T,full.names = T)

#load the headers and data into their own lists
h   = lapply(files, fread,skip = 1,nrow = 0)
dat = lapply(files, fread,skip = 3,header = F,na.strings=c('-9999'))

ls()

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#df=as.data.frame(dat)
#make all the lists into one dataframe
df = dat[[1]]
for (i in 2:length(dat)) {
  df = rbind.fill(df,dat[[i]])
}

#create a timestamp from the date and time
df$ts = as.POSIXct(x = paste(df$date,df$time,sep = ' '),tz = 'UTC')
df = df[!duplicated(df$ts),]


#take the min and max date rounding to the nearest full month
mindate = floor_date(min(df$ts),unit = 'month')
maxdate = ceiling_date(max(df$ts),unit = 'month')

#create a timestamp variable every half hour to the full months
ts = seq(from = mindate,to = maxdate,by = 60*30)
ts = as.data.frame(ts)

ts = seq(min(df$ts),max(df$ts),by = 60*30)
ts = as.data.frame(ts)


#merge with the flux data frame to create NAs where data is missing
df = merge(ts,df,by = 'ts',all.x = T)


#save off flux data
write.csv(df,'C:/Users/klynoe/Documents/pond_inlet/R_outputs/pond_inlet_fluxes_merged_2024_07_11.csv',row.names = F)


################
####SINGLE FILE EDDYPRO PREPARATION####
####################################

fp = 'C:/Users/klynoe/Documents/pond_inlet/eddypro/full_output/2024/07/'### CHANGE MONTH TO WHICH YOU WANT TO LOAD
file = list.files(path = fp,pattern = '*full_output.+csv$',recursive = T,full.names = T)


#load the headers and data into their own lists
h   = lapply(file, fread,skip = 1,nrow = 0)
dat = lapply(file, fread,skip = 3,header = F,na.strings=c('-9999'))

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}
#turn the data into a dataframe
df =as.data.frame(do.call(cbind, dat))

#save off flux data -----> remember to rename the output file as desired
write.csv(df,'C:/Users/klynoe/Documents/pond_inlet/R_outputs/pond_inlet_fluxes_merged_2024_07_11.csv',row.names = F)

############################################
##########################################################



``````````````
# Load and merge biomet data

met = fread('C:/Users/klynoe/Desktop/pondinlet/R_output/pond_inlet_met_merged.csv')



#create a timestamp from the date and time

met$ts = met$TIMESTAMP
  
  #as.POSIXct(x = paste(met$date,met$time,sep = ' '),tz = 'UTC')

#merge datasets

df= merge(met,df,by='ts',all = T)



co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-75,100),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
 # scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
  
ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,2),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
 # scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))



df$Time= df$ts



co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,co2_flux),size = .15)+
  scale_y_continuous(limits = c(-10,6),
                     expression(bold('NEE ('*mu*mol~CO[2]~m^-2*s^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0),
        text = element_text(size = 8),
        axis.text = element_text(size = 8),
        panel.background = element_rect(fill = 'transparent'))
co2

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,ch4_flux*43.2),size = .15)+
  scale_y_continuous(limits = c(-1,8),
                     expression(bold(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7),
        text = element_text(size = 8),
        axis.text = element_text(size = 8),
        panel.background = element_rect(fill = 'transparent'))
ch4


#png(filename = 'C:/Permafrost Pathways/reallyfrigginmintdewd.png',width = 4,height = 3,units = 'in',bg = 'transparent',res = 1500)
#plot_grid(co2,ch4,nrow = 2, align = "v")
#dev.off()


df$Time= df$ts

#plot co2 with qc colors
co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,co2_flux))+
  scale_y_continuous(limits = c(-12,12),
                     expression(bold('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
co2

#plot co2 with qc=2 removed
co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-40,20),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
co2

#plot ch4 with qc colors
ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(Time,ch4_flux*43.2))+
  scale_y_continuous(limits = c(-1,9),
                     expression(bold(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')')))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4


#plot ch4 with qc=2 removed

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,2),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4

#plot H with colored Temp

H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(aes(ts,H,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
H

#plot H with qc=2 removed
H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_H<2,],aes(ts,H,color=qc_H))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
H

#plot LE with colored Temp
LE = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_LE<2,],aes(ts,LE,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-20,75),
                     expression('LE (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))
LE


#plot LE with qc=2 removed
LE = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_LE<2,],aes(ts,LE,color=qc_LE))+
  scale_y_continuous(limits = c(-20,150),
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
  scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))

H = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_H<2,],aes(ts,H,color=air_temperature-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_y_continuous(limits = c(-120,150),
                     expression('H (W/m^2 )'))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))

ch4 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_ch4_flux<2,],aes(ts,ch4_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-1,7),
                     expression(CH[4]~flux~'(mg '*CH[4]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 7))
ch4


co2 = ggplot(data = df)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=df[df$qc_co2_flux<2,], aes(ts,co2_flux*43.2,color=air_temperature-273.15))+
  scale_y_continuous(limits = c(-60,150),
                     expression('NEE (mg '*CO[2]*'-C '*m^-2*h^-1*')'))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-26','2024-10-18')))+
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

windRose(mydata = df,ws = 'co2_flux',wd = 'wind_dir')


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

plot(met$ts,met$TS_2_38_1_1_1,ylim = c(-2,25));abline(h=0)
points(met$ts,met$TS_2_38_1_2_1,col='red')
points(met$ts,met$TS_2_38_1_3_1,col='orange')
points(met$ts,met$TS_2_38_2_1_1,col='yellow')
points(met$ts,met$TS_2_38_2_2_1,col='green')
points(met$ts,met$TS_2_38_2_3_1,col='blue')
points(met$ts,met$TS_2_38_2_4_1,col='cyan')
points(met$ts,met$TS_2_38_2_5_1,col='purple')
points(met$ts,met$TS_2_38_2_6_1,col='pink')
points(met$ts,met$TS_2_38_2_7_1)
points(met$ts,met$TS_2_38_2_8_1,col='red')
points(met$ts,met$TS_2_38_2_9_1,col='orange')


#####BIOMET PLOTS#################


#create a timestamp from the date and time of met

#met$ts = as.POSIXct(x = paste(met$date,met$time,sep = ' '),tz = 'UTC')

#Air Temp

ggplot(data = met)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_point(data=met,aes(ts,TA_2_1_1_1_1-273.15,color=TA_2_1_1_1_1-273.15))+
  scale_color_viridis() + guides(col=guide_legend("Degrees C"))+
  scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  scale_y_continuous(limits = c(-30,30))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))




ggplot(data = met)+theme_bw()+geom_abline(slope = 0,intercept = 0)+
  geom_line(data=met,aes(ts,PPFD_7_21_1_1_1,color='Par'))+
  geom_line(data=met,aes(ts,PPFDR_7_23_1_1_1,color='Par Reflected')) +
  scale_x_datetime(limits = as.POSIXct(c('2024-07-25','2024-10-20')))+
  scale_y_continuous(limits = c(0,1300))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))


ggplot(data = df)+theme_bw()+geom_abline(slope = 1,intercept = 0)+
  geom_point(aes(ch4_flux.x*43.2,ch4_flux.y*43.2))+
  theme(plot.margin = margin(t = 1,r = 1,b = 0,l = 0))+
  scale_x_continuous(limits=c(-2,7.5))+
  scale_y_continuous(limits=c(-2,7.5))


#all the data merged
m = merge(df,met,by='ts',all=T)

names(m)

m$g = (m$SHF_Avg_1+m$SHF_Avg_1)/2

ggplot(data = m,aes(RN_Avg-g,LE+H))+theme_bw()+geom_abline(slope = 1,intercept = 0)+
  geom_point()+
  geom_smooth(method = 'lm')

summary(lm(LE+H~RN_Avg-g,data = m))
 


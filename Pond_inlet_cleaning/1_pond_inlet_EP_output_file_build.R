

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
####FOR SINGLE MONTH FILE PREPARATION SEE 2ND CHAPTHER 
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


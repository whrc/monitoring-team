rm(list = ls())

library(data.table)
Sys.setenv(TZ='UTC')
setwd('C:/Users/karndt.WHRC/Desktop/sites/YKD/Ameriflux YK/unburned/') #use this to set your directory

#Prep for Reddy for an initial gap filling
dat = fread('./ykd_unburned_2019_2023_gf.csv')

#time variables #########################################################
dat$ts = as.POSIXct(dat$ts)

Year = format(dat$ts,'%Y')
DoY  = format(dat$ts,'%j')
h    = as.numeric(format(dat$ts,'%H')) #full hours
h.5  = as.numeric(ifelse(format(dat$ts,'%M') == '00',0,0.5)) #half hour decimals
Hour = h+h.5 #Hour in the expected format

dat$nee = ifelse(is.na(dat$co2_flux.c),dat$rfnee,dat$co2_flux.c)
dat$ch4 = ifelse(is.na(dat$ch4_flux.c),dat$rfch4,dat$ch4_flux.c)

#Reddy df
reddy = data.frame(Year,DoY,Hour,
                   dat$nee,
                   dat$ch4,
                   dat$H.c,
                   dat$LE.c,
                   dat$`u*`,
                   dat$rad.eramod,
                   dat$airt.eramod,
                   dat$TS_2_1_1.c,
                   dat$rh.eramod)
names(reddy) = c('Year','DoY','Hour','NEE','CH4','H','LE','Ustar','Rg','Tair','Tsoil','RH')

#check the assumptions
summary(reddy$Rg) #no negatives allowed
summary(reddy$RH) #no values over 100 allowed

#run these if needed
reddy$Rg = reddy$Rg - min(reddy$Rg)
reddy$Rg = ifelse(reddy$Rg < 0,0,reddy$Rg) #set negatives to 0
reddy$RH = ifelse(reddy$RH > 100,100,reddy$RH)

#add the second header row and resave as a txt
h2 = c('--','--','--','umolm-2s-1','nmolm-2s-1','Wm-2','Wm-2','ms-1','Wm-2','DegC','DegC','%')
names(h2) = names(reddy)
h2 = as.character(h2)
reddy2 = rbind(h2,reddy)
reddy2$Year = as.character(reddy2$Year)
reddy2$DoY  = as.character(reddy2$DoY)
reddy2[1,1] = '--'
reddy2[1,2] = '--'

reddy2 = reddy2[complete.cases(reddy2$Year),]

write.table(x = reddy2,file = './reddy_ykdub.txt',row.names = F,sep = '\t',quote = F,na = '-9999')

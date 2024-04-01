library(tidyverse)

## Loads met data without units

h   = fread("IQtundra2_met_edited - Copy.dat", nrow=0)
dat = fread("IQtundra2_met_edited - Copy.dat", skip = 2,header = F,
            na.strings=c('-9999'))

## assign headers to data

names(dat) = names(h)

## select relevant vars

biomet <- select(dat, TIMESTAMP, SWIN, SWOUT, LWIN, LWOUT, RH, RN, PPFD, 
                   TA)

biomet <- separate(biomet, TIMESTAMP, c("TIMESTAMP_1", "TIMESTAMP_2"),
                   sep = " ")
biomet$TIMESTAMP_2 <- replace_na(biomet$TIMESTAMP_2, "00:00:00")


## coerece to char to add units as a new row

biomet %<>% lapply(as.character) %>% data.frame() %>% 
  add_row(TIMESTAMP_1 = "[yyyy-mm-dd]", TIMESTAMP_2 = "[HHMM]", SWOUT = "[W+1m-2]", 
                   SWIN = "[W+1m-2]", LWIN = "[W+1m-2]", LWOUT = "[W+1m-2]",
                   RN = "[W+1m-2]", PPFD = "[W+1m-2]", TA = "[C]", RH = "[%]", 
                   .before=1)



##read in 

write.csv(biomet, "IQ2_biomet.csv", row.names=FALSE)


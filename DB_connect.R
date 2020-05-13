
#### This script is meant to import and clean up data from the Moberly Lake Database #
#### This can be run in other scripts with source("Mob_Db_connect.R") #
#### NOTE: you need to be running the 32-bit version of R to be able to connect to the Access Db, 
#### depending on the driver installed on your computer. 
#         You can do this with Tools -> Global Options -> General and select the 32-bit version in "R version"


### Author: Kristen Peck, Dec-2017

#install.packages("RODBC", "dplyr", "lubridate")

library(RODBC)
library(dplyr)
library(lubridate)
library(tidyr)

#Open "channel" to database to extract data tables. Close connection when done #
# Troubleshooting Issues:
# CHECK NAME OF MASTER DB and file path if this doesn't work. 
# Try the other "bit" version of R (32-bit or 64-bit)
# after "refresh" the driver may not be installed correctly. 
# Read thru this site and see if it helps: https://docs.microsoft.com/en-us/office/troubleshoot/access/cannot-use-odbc-or-oledb
# I installed a 64-bit driver and it solved my connection issues

ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
	DBQ=//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/Data/Database/Moberly Fish Database-MASTER/Moberly Fish Database-MASTER.accdb")


sqlTables(ch,tableType = "TABLE")["TABLE_NAME"]

effort <- sqlFetch(ch, "TABLE - Effort Information")
env <- sqlFetch(ch,"TABLE - Environmental Info")
catch <- sqlFetch(ch,"TABLE - All Fish Capture Measurements")
bycatch <- sqlFetch(ch,"TABLE - By-Catch Summary Capture Info")
LT.ID <-  sqlFetch(ch,"TABLE - Specific Fish ID Info")

odbcCloseAll()



# ACTIONS TO DO: ####
# - mark whether fish is recap or not in any given year to quickly summarize



#### EFFORT ####

effortR <- effort %>% 
  mutate(field.ID = as.character(EffortFieldNumber),yr = year(EffortEndDateTime), 
         month = month(EffortEndDateTime),
         day = as.numeric(day(EffortEndDateTime)),
         season = factor(case_when(month %in% c(12,1,2) ~ "winter",
                                   month %in% c(3,4,5) ~ "spring",
                                   month %in% c(6,7,8) ~ "summer",
                                   month %in% c(9) & day < 25 ~ "summer",
                                   month %in% c(9) & day >= 25 ~ "fall", #in 2008, 2009 the fall survey started early
                                   month %in% c(10,11) ~ "fall"), ordered=T, 
                         levels = c("winter","spring","summer","fall"))) %>% 
  mutate(soaktime = round(difftime(EffortEndDateTime, EffortStartDateTime, units = "hours"),2)) %>% 
  select(EffortAutoNumber,field.ID, st.datetime=EffortStartDateTime, 
         end.datetime=EffortEndDateTime, yr, month, 
         season, soaktime, shoal=EffortSpawningShoal, UTMZ=EffortSiteID_UTMZone_End, 
         UTME=EffortSiteID_Easting_End, UTMN=EffortSiteID_Northing_End, 
         survey.type=EffortSurveyType, gear.type=EffortGearType,
         mesh=EffortMeshSize, net.cond=EffortNetCond, bottom.depth1=EffortBottomDepth1, 
         bottom.depth3 = EffortBottomDepth3, gear.depth.top = EffortGearDepthTop, 
         gear.depth.bot=EffortGearDepthBottom)
str(effortR)




#### Effort QA####

#which gillnetting efforts have a soaktime of 0? 
effortR %>% 
  filter(soaktime <= 0) %>% 
  filter(gear.type %in% c("RIC6 SGN - 6 Panel Sinking Gillnet","RIC6 FGN - 6 Panel Floating Gillnet",
                          "RIC7 SGN - 7 Panel Sinking Gillnet","SLIN - Spring Littoral Index Netting Gillnet"))
#which efforts do not have their locations defined? Probably lots at the moment, should go back and correct these
effortR %>%
  filter(UTME == 111111)

#visual check to see that months were correctly classified into seasons
arrange(unique(effortR[,c("month","season")]), by = month)

#any rows not fall into a season?
which(is.na(effortR$season))

#These Effort IDs have a duplicate time/date that is not shown here. 
# Go look in the database for the duplicates and assess. Some of these
# may be a result of two crews working at the same time in spring, 
# so they are fine.
effortR[duplicated(paste(effortR$st.datetime,effortR$end.datetime)),]





#### ENV ####

#tidy up ENV data 
envR <- env %>% 
  select(EffortAutoNumber=Effort_AutoNumber, method = ENVSamplingTool, secchi=ENV_SecchiDepth, temp=ENV_H2OTemp,
         wind.dir = ENV_WindDirection, wind.spd = ENV_WindSpeed)
envR
### Temp join Env measures with Effort

effortR <- effortR %>% 
  left_join(envR, by="EffortAutoNumber")

# ***note: in cases where there are multiple env measures per effort, 
# this merge will create duplicate effort IDs. Which will mess up 
# other matches. Currently don't have any, but need to check if there are any
anyDuplicated(effortR$EffortAutoNumber)





#### CATCH and BYCATCH ####

#get sex and age from LT.ID table and clean up catch table

yr.select <- 2019

LT.IDR <- LT.ID %>% 
  mutate(sex = as.character(FishSex)) %>%
  mutate(yr.class = year(FishFinal_Age_Date)-FishFinal_Age) %>% 
  mutate(ageatyr.select = yr.select-yr.class) %>% 
  mutate(hatchery = ifelse(!is.na(LTCohortYr), "yes","no")) %>% 
  select(LTFishIDAutonumber, sex, yr.class, ageatyr.select, hatchery)

# freq of fish of diff ages in the selected year
hist(LT.IDR$ageatyr.select)

#attribute fish year class from LT.ID table. This should be interpreted as =/- 0.5-1 year
################## RICKS MODS ################## to calculate age if it survived to 2019

#add sex, whether hatchery, and yr.class to catch
catchR <- catch %>% 
  select(EffortAutoNumber=EffortAutoNumber_AllFish, LTFishIDAutonumber=LTFishID_Autonumber, species=CaptureSpecies, 
         FL=`CaptureFork Length`,WT=CaptureWeight, maturity=CaptureMaturity, 
         fate=CaptureFate, datetime = CaptureDate) %>% 
  mutate(condition=(100000*WT)/(FL^3), yr=year(datetime), 
         species = as.character(species)) %>% 
  mutate(count = ifelse(species %in% "NFC",0,1)) %>% 
  left_join(LT.IDR, by= "LTFishIDAutonumber")

str(catchR)  


##### recapture histories ####

catch.history <- effortR %>% 
  dplyr::select(EffortAutoNumber,season,shoal,survey.type,gear.type) %>% 
  full_join(catchR) %>% 
  filter(species %in% "LT") %>% 
  mutate(freq = case_when(fate %in% c("a",NA) ~ 1,
                          fate %in% c("m","m?") ~ -1)) %>%   #assume that suspected deaths are real
  arrange(datetime)
  
#QA to check if fish were mis-recorded as dead and then found alive
qa.freq <- catch.history %>% 
  group_by(LTFishIDAutonumber) %>% 
  summarize(qa.freq = paste(freq, collapse = ""), qa.sex = paste(sex,collapse = ""))
unique(qa.freq$qa.freq) #none of these should havea death in the middle of the series
unique(qa.freq$qa.sex) #none of these should switch sex in the middle of the series

# qa.freq[which(qa.freq$qa.freq %in% c("-11", "11-11111")),]
# catch.history %>% 
#   filter(LTFishIDAutonumber %in% c(143, 219 , 12274)) %>% 
#   arrange(LTFishIDAutonumber, datetime)
# 
#fixed 143 (suspected death from poor release in 2013)
#fixed 219, where the date of lethal sample was mis-recorded
#fixed 12274 (581) -> this fish (caught Aug 22, 2017) should have been fish #341. Changed

#catches per year, all seasons, all types:
(catch.hist.byyr <- catch.history %>%  
  group_by(LTFishIDAutonumber, yr) %>% 
  summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
  arrange(LTFishIDAutonumber))
str(catch.hist.total)

#catches by year and season, all types:
(catch.hist.byseasonyr <- catch.history %>%  
  filter(season %in% c("spring", "summer", "fall")) %>% 
  mutate(season.num = case_when(season %in% "spring" ~ "1spring",
                                season %in% "summer" ~ "2summer",
                                season %in% "fall" ~ "3fall")) %>% 
  mutate(season.yr = factor(paste0(yr,"-",season.num), ordered=T)) %>% 
  group_by(LTFishIDAutonumber, season.yr) %>% 
  summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
  mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
  arrange(LTFishIDAutonumber)) 


#all seasons, all recap types:
(catch.hist.wide <- spread(data=catch.hist.byseasonyr, key=season.yr, value = tot.catches,
                           fill=0))

cols <- names(catch.hist.wide)[4:ncol(catch.hist.wide)]
catch.hist.wide$ch <- do.call(paste, c(catch.hist.wide[cols],sep=""))
headtail(catch.hist.wide)



#catches on spawning shoals only (excluding holding pen):
catch.hist.spawner <- catch.history %>%  
    filter(survey.type %in% "Spawner Sampling/Tagging", 
           gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                            "Seine Net", "Angling")) %>% 
    group_by(LTFishIDAutonumber, yr) %>% 
    summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
    mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
    arrange(LTFishIDAutonumber, yr) 
catch.hist.spawner

ch.spawner <- catch.hist.spawner %>% 
  spread(key=yr, value = tot.catches,fill=0) 

cols <- names(ch.spawner)[4:ncol(ch.spawner)]
ch.spawner$ch <- do.call(paste, c(ch.spawner[cols],sep=""))
headtail(ch.spawner)

str(ch.spawner)

#see if any fish in the dataset have no captures
no.catch <- paste(rep(0,length(4:ncol(ch.spawner))-1),collapse ="")
which(ch.spawner$ch == no.catch)





#### bycatch ####
unique(bycatch$ByCatchSpecies)

bycatchR <- bycatch %>% 
  select(EffortAutoNumber=EffortAutoNumber_ByCatch, species=ByCatchSpecies, 
         count = ByCatchCount,
         datetime = ByCatchDate) %>% 
  mutate(yr = year(datetime), species = as.character(species))
str(bycatchR)




#merge catches:

#catch.merge <- catch[,c("EffortAutoNumber","CaptureIDAutoNumber","LTFishID_Autonumber","species",
#                         "count","FL","WT","condition","yr.class","cohort","sex","in.offshore","maturity","fate","stomach1",
#                         "stomach.comm1","comments","SummaryCaptureIDAutoNumber")]
# bycatch.merge <- bycatch[,c("EffortAutoNumber","SummaryCaptureIDAutoNumber","LTFishID_Autonumber",
#                             "species","count","FL","WT","condition","yr.class","cohort","sex","in.offshore","maturity","fate","stomach1",
#                             "stomach.comm1","comments","CaptureIDAutoNumber")]
str(catchR)
str(bycatchR)
unique(bycatchR$count)

catch.all <- catchR %>% 
  full_join(bycatchR, by=c("EffortAutoNumber","species","count", "yr")) #for some reason this seems to be eliminating some rows of bycatch. See effort 3516 to see what I mean
str(catch.all)

catch.all$catchID <- 1:nrow(catch.all)

str(catch.all)


### Merge efforts with catches ##

effort.catch <- effortR %>% 
  full_join(catch.all) %>% 
  mutate(species = ifelse(is.na(species),"NFC", species)) # insert "NFC" where there is no corresponding catch for effort

unique(effort.catch$species)




### Small Queries ####

#how many fish were caught on shoals in 2019?
str(catch.all)

catch.all.2019 <- catch.all %>% 
  filter(yr %in% 2019) %>% 
  filter(species %in% "LT")

length(unique(catch.all.2019$LTFishIDAutonumber))

#how many were male?

catch.all.2019m <- catch.all %>% 
  filter(yr %in% 2019) %>% 
  filter(species %in% "LT") %>% 
  filter(sex %in% "m")

length(unique(catch.all.2019m$LTFishIDAutonumber))




###############################################
###############################################
##########    QA     ########################## NOT YET UPDATED KP/10-March-2020
###############################################
###############################################



# ### QA effort 
# 
# ## Check list of surveys: 
# str(effortR)
# effortR$season.yr.survey <- paste(effortR$yr, effortR$season, effortR$survey.type)
# effortR <- effortR[order(effortR$season.yr.survey),]
# 
# uniq <- data.frame(table(effortR$season.yr.survey))
# colnames(uniq) <- c("Sampling Event","Number of Efforts")
# uniq$Year <- substr(uniq[,1], 1,4)
# 
# uniq
# 
# 
# #setwd(Db.connect)
# #write.csv(uniq, "sampling events.csv", row.names=F)
# 
# 
# 
# 
# ### QA catch
# 
# # check for NFCs (efforts without corresponding catch) with additional catches
# #nfcs <- catch.all[which(catch.all$species=="NFC"),"catchID"]
# #catch.all[which(catch.all$catchID==nfcs),]
# #length(nfcs)== nrow(catch.all[which(catch.all$catchID==nfcs),])
# # If there are no errors, this should be "TRUE"
# 
# catch[which(catch$species=="LT"),30:36]
# str(catch)
# 
# 
# 
# 
# 
# ### QA LT.IDs 
# 
# LT <- LT.ID[(which(LT.ID$FishSpecies=="LT")),]
# str(LT)
# 
# #check if any have dec tag, but no hex tag (preferred). Should be "integer(0)"
# which(!is.na(LT$LTPitTag_DEC_ID_1)& is.na(LT$LTPitTag_HEX_ID_1))
# which(!is.na(LT$LTPitTag_DEC_ID_2)& is.na(LT$LTPitTag_HEX_ID_2))
# which(!is.na(LT$LTPitTag_DEC_ID_3)& is.na(LT$LTPitTag_HEX_ID_3))
# 
# #check if any have duplicate tag information (PIT tags)
# #simple:
# dup <- data.frame(table(LT$LTPitTag_HEX_ID_1))
# dup[which(dup[,2]>1),1]
# 
# #complex:
# 
# LT$LTPitTag_HEX_ID_1 <- as.character(LT$LTPitTag_HEX_ID_1)
# LT$LTPitTag_HEX_ID_2 <- as.character(LT$LTPitTag_HEX_ID_2)
# LT$LTPitTag_HEX_ID_3 <- as.character(LT$LTPitTag_HEX_ID_3)
# 
# LT$tags <- paste(LT$LTPitTag_HEX_ID_1, LT$LTPitTag_HEX_ID_2, LT$LTPitTag_HEX_ID_3)
# 
# hex1 <- NA
# length(hex1) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   hex1[i] <- length(which(grepl(LT$LTPitTag_HEX_ID_1[i],LT$tags)== TRUE))
# }
# LT[which(hex1 > 1),c("LTFishIDAutonumber","tags")] 
# #Fish # 642 + 503 not currently fixable, but pay attention to others
# 
# hex2 <- NA
# length(hex2) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   hex2[i] <- length(which(grepl(LT$LTPitTag_HEX_ID_2[i],LT$tags)== TRUE))
# }
# LT[which(hex2 > 1),c("LTFishIDAutonumber","tags")]
# 
# 
# #check if any have duplicate tag information (floy tags)
# #simple:
# dup <- data.frame(table(LT$LTFLOYID1))
# dup[which(dup[,2]>1),1]
# #complex:
# 
# LT$LTFLOYID1 <- as.character(LT$LTFLOYID1)
# LT$LTFLOYID2 <- as.character(LT$LTFLOYID2)
# LT$LTFLOYID3 <- as.character(LT$LTFLOYID3)
# LT$LTFLOYID4 <- as.character(LT$LTFLOYID4)
# 
# LT$floys <- paste(LT$LTFLOYID1, LT$LTFLOYID2, LT$LTFLOYID3, LT$LTFLOYID4)
# 
# floy1 <- NA
# length(floy1) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   floy1[i] <- length(which(grepl(LT$LTFLOYID1[i],LT$floys)== TRUE))
# }
# LT[which(floy1 > 1),c("LTFishIDAutonumber","floys")] 
# 
# floy2 <- NA
# length(floy2) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   floy2[i] <- length(which(grepl(LT$LTFLOYID2[i],LT$floys)== TRUE))
# }
# LT[which(floy2 > 1),c("LTFishIDAutonumber","floys")] 
# 
# floy3 <- NA
# length(floy3) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   floy3[i] <- length(which(grepl(LT$LTFLOYID3[i],LT$floys)== TRUE))
# }
# LT[which(floy3 > 1),c("LTFishIDAutonumber","floys")] 
# 
# floy4 <- NA
# length(floy4) <- nrow(LT)
# for (i in 1:nrow(LT)){
#   floy4[i] <- length(which(grepl(LT$LTFLOYID4[i],LT$floys)== TRUE))
# }
# LT[which(floy4 > 1),c("LTFishIDAutonumber","floys")] 
# 
# 
# 
# 
# # ** indiciate which fish are the unidentifiable re-caps
# # Check out Recap Identifier field
# 
# #****don't use the Recap identifier for now
# LT[,1:6]
# levels(LT$LTRecapIdentifier)
# 
# LT[which(LT$LTRecapIdentifier=="Hatchery Cohort" & is.na(LT$LTCohortYr)),]
# LT[which(!is.na(LT$LTRecapIdentifier)),c("LTFishIDAutonumber","LTRecapIdentifier","LTCohortYr","FishComments")]
# 
# levels(LT$LTRecapIdentifier)
# #aborted- need to clean up this field and make more useful
# 
# 
# 
# #check which Lakers don't have a catch record
# 
# LT[which(LT$LTFishIDAutonumber %in% catch$LTFishID_Autonumber == FALSE),1:6]
# 
# #the problematic records appear to be 9000 to 11552. Not sure what the other ones were.. misfires?
# 
# 
# 
# 
# 
# 
# 
# 
# #### 
# #### TO DO
# ####
# 
# #**write script to give summary of captures of individual fish IDs 
# 
# 
# 
# 
# 
# 
# 
# # QA effort.catch
# 
# event <- levels(as.factor(effort.catch$season.yr.survey))
# 
# dupe <- effort.catch[1:length(event),]; 
# 
# i=3
# effort.catch[which(duplicated(effort.catch[which(effort.catch$season.yr.survey == event[i]),
#                                            c("LTFishID_Autonumber","species","FL","WT","count","sex")])==TRUE),]
# 
# effort.catch$qa.field1 <- paste(effort.catch$season.yr.survey,effort.catch$species,effort.catch$FL,
#                                 effort.catch$WT,effort.catch$sex)
# 
# 
# 
# #QA routines to write:
# #write chunk for list of surveys that have occurred and how many efforts, 
# #catch and by-catch they all have -> for review post-data entry
# #write script for any LTs that have wt but no FL
# #check if duplicate LT.ID'ing parameters
# # translate x,y,z over from site ID
# 
# 
# 
# 
# 
# #for clean "source" run in other scripts:
# 
# 
ls()
 rm("ch","bycatch","catch","env","effort","i", "sexR")
# ls()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 























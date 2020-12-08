
#### This script is meant to import and clean up data from the Moberly Lake Database #
#### This can be run in other scripts with source("Mob_Db_connect.R") #
#### NOTE: you need to be running the 32-bit version of R to be able to connect to the Access Db, 
#### depending on the driver installed on your computer. 
#         You can do this with Tools -> Global Options -> General and select the 32-bit version in "R version"


### Author: Kristen Peck, Dec-2017

#install.packages("RODBC", "dplyr", "lubridate")

library(RODBC)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#Open "channel" to database to extract data tables. Close connection when done #
# Troubleshooting Issues:
# CHECK NAME OF MASTER DB and file path if this doesn't work. 
# Try the other "bit" version of R (32-bit or 64-bit)
# after "refresh" the driver may not be installed correctly. 
# Read thru this site and see if it helps: https://docs.microsoft.com/en-us/office/troubleshoot/access/cannot-use-odbc-or-oledb
# I installed a 64-bit driver and it solved my connection issues

#NOTE: you need to be connected to the network files (VPN) and have access to the FSJ network files

# ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
# 	DBQ=//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/Data/Database/Moberly Fish Database-MASTER/Moberly Fish Database-MASTER.accdb")
# #suddenly not working again! seems to be an issue with the network, since the below works with a copy:


### Connect to Moberly Database ####

ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
	DBQ=C:/Users/krispeck/Documents/R/moberly/Moberly Fish Database-copy03-Dec-2020.accdb")

sqlTables(ch,tableType = "TABLE")["TABLE_NAME"]

effort <- sqlFetch(ch, "TABLE - Effort Information")
env <- sqlFetch(ch,"TABLE - Environmental Info")
catch <- sqlFetch(ch,"TABLE - All Fish Capture Measurements")
bycatch <- sqlFetch(ch,"TABLE - By-Catch Summary Capture Info")
LT.ID <-  sqlFetch(ch,"TABLE - Specific Fish ID Info", na.strings = "NA")

odbcCloseAll()


str(LT.ID)

# ACTIONS TO DO: ####
# - mark whether fish is recap or not in any given year to quickly summarize
# - print a copy of effort and catch for most recent year to QA data entry


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
  dplyr::select(EffortAutoNumber,field.ID, st.datetime=EffortStartDateTime, 
         end.datetime=EffortEndDateTime, yr, month, 
         season, soaktime, shoal=EffortSpawningShoal, UTMZ=EffortSiteID_UTMZone_End, 
         UTME=EffortSiteID_Easting_End, UTMN=EffortSiteID_Northing_End, 
         survey.type=EffortSurveyType, gear.type=EffortGearType,
         mesh=EffortMeshSize, net.cond=EffortNetCond, bottom.depth1=EffortBottomDepth1, 
         bottom.depth3 = EffortBottomDepth3, gear.depth.top = EffortGearDepthTop, 
         gear.depth.bot=EffortGearDepthBottom)
str(effortR)




#### Effort QA ####

#which gillnetting efforts have a soaktime of 0? 
effortR %>% 
  filter(soaktime <= 0) %>% 
  filter(gear.type %in% c("RIC6 SGN - 6 Panel Sinking Gillnet","RIC6 FGN - 6 Panel Floating Gillnet",
                          "RIC7 SGN - 7 Panel Sinking Gillnet","SLIN - Spring Littoral Index Netting Gillnet"))
#which efforts do not have their locations defined? Probably lots at the moment, should go back and correct these
effortR %>%
  filter(UTME == 111111) %>% 
  dplyr::select(EffortAutoNumber, field.ID, end.datetime, UTME, UTMN, survey.type) %>% 
  arrange(desc(end.datetime))

#visual check to see that months were correctly classified into seasons
arrange(unique(effortR[,c("month","season")]), by = month)

#any rows not fall into a season?
which(is.na(effortR$season))

#These Effort IDs have a duplicate time/date in the db (only one of duplicates shown). 
# Go look in the database for the duplicates and assess. Some of these
# may be a result of two crews working at the same time in spring, 
# so those are fine, but this should catch double-entered efforts.
effortR[duplicated(paste(effortR$st.datetime,effortR$end.datetime)),]





#### ENV ####

#tidy up ENV data 
envR <- env %>% 
  dplyr::select(EffortAutoNumber=Effort_AutoNumber, method = ENVSamplingTool, secchi=ENV_SecchiDepth, 
        temp=ENV_H2OTemp, wind.dir = ENV_WindDirection, wind.spd = ENV_WindSpeed)
str(envR)
### Join Env measures with Effort

effortR <- effortR %>% 
  left_join(envR, by="EffortAutoNumber")

# ***note: in cases where there are multiple env measures per effort, 
# this merge will only take one of those env measures. 
#To double-check whether any effort has >1 env measure, 
#change the above to a full_join and run:

#anyDuplicated(effortR$EffortAutoNumber)

#Note also that any env measures NOT associated with an effort won't be there, 
# because these are not currently allowed in the DB

# Secchi measurements:

effortR %>% 
  dplyr::select(yr, month, secchi) %>% 
  filter(!is.na(secchi)) %>% 
  arrange(yr, month)

#Ave. surface temps during spawner survey:

effortR %>% 
  dplyr::select(yr, month, survey.type, temp) %>% 
  filter(survey.type %in% "Spawner Sampling/Tagging") %>% 
  filter(!is.na(temp)) %>% 
  group_by(yr) %>% 
  dplyr::summarize(ave.temp = mean(temp), sd.temp= sd(temp, na.rm=T))




#### CATCH and BYCATCH ####

#get sex and age from LT.ID table to add to general catch table and clean up catch table
# need to select the year of interest (e.g. most recent sampling year) to calculate ages to that date

yr.select <- 2020

LT.IDR <- LT.ID %>% 
  mutate(sex = as.character(FishSex)) %>%
  mutate(yr.class = year(FishFinal_Age_Date)-FishFinal_Age) %>% 
  mutate(ageatyr.select = yr.select-yr.class) %>% 
  mutate(hatchery = ifelse(LTRecapIdentifier=="Hatchery Cohort", "yes","no")) %>% 
  dplyr::select(LTFishIDAutonumber, sex, yr.class, ageatyr.select, hatchery)


# theoretical freq of fish of diff ages in the selected year (not all would be alive still)
hist(LT.IDR$ageatyr.select)

#attribute fish year class from LT.ID table. This should be interpreted as =/- 0.5-1 year old,
#since some age samples were collected in spring, some in summer, some in fall...
# and all are likely somewhat inaccurate


#add sex, whether hatchery, and yr.class to catch
catchR <- catch %>% 
  dplyr::select(EffortAutoNumber=EffortAutoNumber_AllFish, LTFishIDAutonumber=LTFishID_Autonumber, 
                species=CaptureSpecies, FL=`CaptureFork Length`,WT=CaptureWeight, 
                maturity=CaptureMaturity, fate=CaptureFate, datetime = CaptureDate) %>% 
  mutate(condition=(100000*WT)/(FL^3), yr=year(datetime), 
         species = as.character(species)) %>% 
  mutate(count = ifelse(species %in% "NFC",0,1)) %>% 
  left_join(LT.IDR, by= "LTFishIDAutonumber")

str(catchR)  



#### bycatch ####
unique(bycatch$ByCatchSpecies)

bycatchR <- bycatch %>% 
  dplyr::select(EffortAutoNumber=EffortAutoNumber_ByCatch, species=ByCatchSpecies, 
         count = ByCatchCount,datetime = ByCatchDate) %>% 
  mutate(yr = year(datetime), species = as.character(species), fate= "e")
str(bycatchR)

#add catchR colnames to bycatchr for rbind (probably a better way of doing this...)


list.names <- setdiff(names(catchR),names(bycatchR))
temp.df <- data.frame(matrix(nrow = nrow(bycatchR),ncol=length(list.names)))
colnames(temp.df) <- list.names

bycatchRmerge <- cbind(bycatchR, temp.df)

#merge with catchR

catch.all <- rbind(catchR, bycatchRmerge)

catch.all$catchID <- 1:nrow(catch.all) #**** what was this for??

str(catch.all)


### Merge efforts with catches ##

effort.catch <- effortR %>% 
  full_join(catch.all) %>% 
  mutate(species = ifelse(is.na(species),"NFC", species)) # insert "NFC" where there is no corresponding catch for effort

unique(effort.catch$species)



#do all catch dates match the effort dates? seems like they do, 
#since no extra column is made for catch datatime


### Small Queries ####

#print out effort and catch sheet for select year for visual comparison to datasheets

yr.select <- 2017

(effort.QA <- effortR %>% 
  filter(yr %in% yr.select, season %in% "fall") %>% 
  mutate(field.IDnum = as.numeric(field.ID)) %>% 
  select(field.ID, field.IDnum, gear.type, mesh,shoal,UTME, UTMN, bottom.depth3, st.datetime, 
         end.datetime, temp) %>% 
  arrange(field.IDnum))
#survey.type %in% "Spawner Sampling/Tagging"



(catch.QA <- effort.catch %>% 
    filter(yr %in% yr.select, season %in% "fall") %>% 
    mutate(field.IDnum = as.numeric(field.ID)) %>% 
    select(LTFishIDAutonumber, datetime, EffortAutoNumber, field.IDnum,species, count, FL, WT, sex) %>% 
    arrange(field.IDnum))



#how many individual fish were caught on shoals in a given year?
str(catch.all)

yr.select <- 2020

(catch.all.yrselect <- catch.all %>% 
  filter(yr %in% yr.select) %>% 
  filter(species %in% "LT"))

(LTcaught.yrselect <- length(unique(catch.all.yrselect$LTFishIDAutonumber)))


#how many were male?

nrow(catch.all.yrselect %>% 
  filter(sex %in% "m"))

#small plots

ggplot(catch.all.yrselect)+
  geom_histogram(aes(x=FL,fill=sex), binwidth=30, col="black")+
  ggtitle(label=paste(yr.select, "spawner FL"))





###    QA     ########################## PARTIALLY UPDATED KP/7-Dec-2020



### QA effort ####
# 
# ## Check list of surveys: 
str(effortR)
effortR$season.yr.survey <- paste(effortR$yr, effortR$season, effortR$survey.type)
effortR <- effortR[order(effortR$season.yr.survey),]

uniq <- data.frame(table(effortR$season.yr.survey))
colnames(uniq) <- c("Sampling Event","Number of Efforts")
uniq$Year <- substr(uniq[,1], 1,4)

uniq


 
### QA catch #### 

#check for NFCs (efforts without corresponding catch) with additional catches
#except that I put the NFCs in at the catch.effort level, so this needs updating

(nfcs <- catch.all[which(catch.all$species=="NFC"),"catchID"])

length(nfcs) == nrow(catch.all[which(catch.all$catchID %in% nfcs),])
#If there are no errors, this should be "TRUE"






### QA LT.IDs ####

#by year:
unique(effort.catch$survey.type)

(check.yr <- effort.catch %>% 
  filter(survey.type %in% "Non-Random Sampling") %>% 
  filter(yr %in% 2017, season %in% "fall") %>%
  filter(species %in% "LT") %>% 
  select(-c(survey.type,gear.type)) %>% 
  mutate(fieldIDnum = as.numeric(field.ID)) %>% 
  group_by(fieldIDnum) %>% 
  summarize(length(fieldIDnum)) %>% 
  arrange(fieldIDnum))

#"Non-Random Sampling"
#"Spawner Sampling/Tagging"

unique(effort.catch$survey.type)

#2020 - good
#2019 - totals correct; net totals good
#2018 - totals correct; net totals good
#2017 - seems good - rechecked data sheets and totals


length(unique(catch$LTFishID_Autonumber)) #this is how many records should be in the LTID table
nrow(LT.ID) #this is how many we actually have

#pick out LT IDs that have no associated catch:
homelessLT <- anti_join(LT.ID,catch, by=c( "LTFishIDAutonumber"="LTFishID_Autonumber"))

homelessLT$LTFishIDAutonumber

write.csv(homelessLT, "homelessLT.csv")

dup.select <- "LTFLOYID1"

LT.QA <- LT.ID %>% 
  filter(!is.na(dup.select)) 

LT.QA[which(duplicated(LT.QA$LTFLOYID2) == TRUE),c("LTFishIDAutonumber","LTFLOYID2")]


LT.ID[which(duplicated(!is.na(LT.ID$LTFLOYID1))=="TRUE"),c("LTFishIDAutonumber","LTFLOYID1")]

which(LT.ID$LTFLOYID1 == "o - 01917")

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
 rm("ch","bycatch","catch","env","effort",
    "yr.select", "LT.ID", "list.names", "temp.df")
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























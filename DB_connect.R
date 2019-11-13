
#### This script is meant to import and clean up data from the Moberly Lake Database #
#### This can be run in other scripts with source("Mob_Db_connect.R") #


### Author: Kristen Peck, Dec-2017

#install.packages("RODBC","dplyr")

library(RODBC)
library(dplyr)

#Open "channel" to database to extract data tables. Close connection when done #
# CHECK CURRENT NAME OF MASTER DB if this doesn't work
ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
	DBQ=//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/Data/Database/Moberly Fish Database-MASTER/Moberly Fish Database-MASTER.accdb")

#sqlTables(ch,tableType = "TABLE")

effort <- sqlFetch(ch, "TABLE - Effort Information")
env <- sqlFetch(ch,"TABLE - Environmental Info")
catch <- sqlFetch(ch,"TABLE - All Fish Capture Measurements")
bycatch <- sqlFetch(ch,"TABLE - By-Catch Summary Capture Info")
LT.ID <-  sqlFetch(ch,"TABLE - Specific Fish ID Info")

odbcCloseAll()

##################
### Workspaces ###
##################

#Db.connect <- "//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/R Scripts & Outputs/2017-18/Db_connect/"

getwd()

###################################
#### calculate some common fields #
###################################

#EFFORT#
str(effort)

effort$field.ID <- as.character(effort$EffortFieldNumber)
effort$start.date.time <- effort$EffortStartDateTime
effort$end.date.time <- effort$EffortEndDateTime
effort$yr <- as.integer(substr(effort$EffortEndDateTime,1,4))
effort$month <- substr(effort$EffortEndDateTime,6,7)
effort$season <- ifelse(effort$month=="12"|effort$month=="01"|effort$month=="02","4winter",
                        ifelse(effort$month=="03"|effort$month=="04"|effort$month=="05","1spring",
                               ifelse(effort$month=="07"|effort$month=="08"|effort$month=="09","2summer",
                                      ifelse(effort$month=="10"|effort$month=="11","3fall",NA)))
)
#exceptions:

effort[which(effort$yr==2008 & effort$month=="09"), "season"] <- "3fall"
effort[which(effort$yr==2009 & effort$month=="09"), "season"] <- "3fall"


#for some reason this does not work... I think date has to be converted to a factor 
#and ordered but blech...
#effort$month.day <- format(effort$start.date.time, "%b-%d")
#effort$season <- ifelse(effort$month.day >= "Dec-01" & 
#	effort$month.day <= "Feb-28","winter",
#	ifelse(effort$month.day >= "Mar-01" & 
#	effort$month.day <= "Apr-30", "spring",
#	ifelse(effort$month.day >= "Jul-01" & 
#	effort$month.day <= "Sept-15","summer",
#	ifelse(effort$month.day >= "Sept-15" & 
#	effort$month.day <= "Oct-31","fall",NA)))
#)

effort[,c("start.date.time","month","season")]

#yrs <- levels(as.factor(effort$yr))
#par(mfrow=c(8,2), mar=c(2,2,2,2))
#for (i in 1:length(yrs)){
#hist(effort[which(effort$yr==yrs[i]),"end.date.time"], breaks="months", main=yrs[i], xlab=NULL, ylab=NULL)
#}

effort$soaktime <- round(difftime(effort$EffortEndDateTime,
                                  effort$EffortStartDateTime, units = "hours"),2)
effort$shoal <- effort$EffortSpawningShoal
effort$UTMZ <- effort$EffortSiteID_UTMZone_End
effort$UTME <- effort$EffortSiteID_Easting_End
effort$UTMN <- effort$EffortSiteID_Northing_End
effort$survey.type <- effort$EffortSurveyType
effort$gear.type <- effort$EffortGearType
effort$mesh <- effort$EffortMeshSize
effort$net.cond <- effort$EffortNetCond
effort$bottom.depth1 <- effort$EffortBottomDepth1
effort$bottom.depth3 <- effort$EffortBottomDepth3
effort$gear.depth.top <- effort$EffortGearDepthTop 
effort$gear.depth.bot <- effort$EffortGearDepthBottom
effort$effort.comments <- effort$EffortComments 

#QA#

#effort[which(effort$soaktime <=0),]
#effort[which(effort$UTME == 111111),]
#Visual: effort[,c("month","season")]
#which(effort[,"season"]=="NA") #should be "integer(0). If not, 
#there is an uncoded season in the effort data 


# subset:
effort <- effort[,c("EffortAutoNumber","field.ID","yr","season","start.date.time",
                    "end.date.time","soaktime","shoal","UTMZ","UTME","UTMN","survey.type",
                    "gear.type","mesh","net.cond","bottom.depth1","bottom.depth3",
                    "gear.depth.top","gear.depth.bot","effort.comments")]



### Merge Env measures with effort

env$EffortAutoNumber <- env$Effort_AutoNumber
env$method <- env$ENVSamplingTool 
env$secchi <- env$ENV_SecchiDepth
env$temp <- env$ENV_H2OTemp
env$wind.dir <- env$ENV_WindDirection
env$wind.spd <- env$ENV_WindSpeed 
env$env.comments <- env$ENV_Comments

env.merge <- env[,c("EffortAutoNumber","ENVAutoNumber","method","secchi",
                    "temp","wind.dir","wind.spd","env.comments")]
# ***note: in cases where there are multiple env measures per effort, 
# this merge will create duplicate effort IDs. Which will mess up 
# other matches. 
anyDuplicated(env.merge$EffortAutoNumber)

effort <- merge(effort,env.merge, all.x=TRUE)

#QA# 

effort[duplicated(paste(effort$start.date.time,effort$end.date.time)),1:16]
#These Effort IDs have a duplicate time/date that is not shown here. 
# Go look in the database for the duplicates and assess. Some of these
# may be a result of two crews working at the same time in spring, 
# so they are fine.


#### Merge catches (measured and unmeasured)

catch$EffortAutoNumber <- catch$EffortAutoNumber_AllFish
catch$species <- catch$CaptureSpecies
catch$count <- 1
catch$FL <- catch[,"CaptureFork Length"]
catch$WT <- catch$CaptureWeight
catch$condition <- (100000*catch$WT)/(catch$FL^3)
catch$in.offshore <- catch$CaptureShore
catch$maturity <- catch$CaptureMaturity
catch$fate <- catch$CaptureFate
catch$stomach1 <- catch$CaptureStomachCnts1
catch$stomach.comm1 <- catch$CaptureStomachComm1
catch$comments <- catch$CaptureComments

#attribute fish sex from LT.ID table
catch$sex <- NA
LT.ID$FishSex <- as.character(LT.ID$FishSex)
for (i in 1:nrow(catch)){
  catch$sex[i] <- LT.ID[which(LT.ID$LTFishIDAutonumber==catch$LTFishID_Autonumber[i]),"FishSex"]
}

#attribute fish year class from LT.ID table. This should be interpreted as =/- 0.5-1 year
################## RICKS MODS ################## to calculate age if it survived to 2017
LT.ID$yr.class <- as.numeric(substr(LT.ID$FishFinal_Age_Date,1,4))-LT.ID$FishFinal_Age
LT.ID$age18 <- 2018 - as.numeric(substr(LT.ID$FishFinal_Age_Date,1,4)) + LT.ID$FishFinal_Age

################## END RICKS MODS ##############
LT.ID$yr.class <- as.numeric(substr(LT.ID$FishFinal_Age_Date,1,4))-LT.ID$FishFinal_Age

catch$yr.class <- NA
for (i in 1:nrow(catch)){
  catch$yr.class[i] <- LT.ID[which(LT.ID$LTFishIDAutonumber==catch$LTFishID_Autonumber[i]),"yr.class"]
}

#attribute hatchery cohort from LT.ID table
catch$cohort <- NA
for (i in 1:nrow(catch)){
  catch$cohort[i] <- LT.ID[which(LT.ID$LTFishIDAutonumber==catch$LTFishID_Autonumber[i]),"LTCohortYr"]
}


##bycatch


bycatch$EffortAutoNumber <- bycatch$EffortAutoNumber_ByCatch
bycatch$LTFishID_Autonumber <- NA
bycatch$species <- bycatch$ByCatchSpecies
bycatch$count <- bycatch$ByCatchCount
bycatch$in.offshore <- bycatch$ByCatchShore
bycatch$comments <- bycatch$ByCatchComments

bycatch$FL <- NA
bycatch$WT<- NA
bycatch$condition <- NA
bycatch$yr.class <- NA
bycatch$cohort <- NA
bycatch$maturity<- NA
bycatch$fate<- NA
bycatch$stomach1<- NA
bycatch$stomach.comm1<- NA
bycatch$sex <- NA

bycatch$CaptureIDAutoNumber <- NA
catch$SummaryCaptureIDAutoNumber <- NA


#merge catches:

catch.merge <- catch[,c("EffortAutoNumber","CaptureIDAutoNumber","LTFishID_Autonumber","species",
                        "count","FL","WT","condition","yr.class","cohort","sex","in.offshore","maturity","fate","stomach1",
                        "stomach.comm1","comments","SummaryCaptureIDAutoNumber")]
bycatch.merge <- bycatch[,c("EffortAutoNumber","SummaryCaptureIDAutoNumber","LTFishID_Autonumber",
                            "species","count","FL","WT","condition","yr.class","cohort","sex","in.offshore","maturity","fate","stomach1",
                            "stomach.comm1","comments","CaptureIDAutoNumber")]

catch.all <- rbind(catch.merge, bycatch.merge)
catch.all$catchID <- 1:nrow(catch.all)

str(catch.all)




### Merge efforts with catches ##

effort.catch <- merge(effort, catch.all, all.x=T)

str(effort.catch)

# insert "NFC" where there is no corresponding catch for effort:

effort.catch[which(is.na(effort.catch$catchID)),"species"] <- "NFC"

which(is.na(effort.catch$species)) #should be "integer(0)"






###############################################
###############################################
##########    QA     ##########################
###############################################
###############################################



### QA effort 

## Check list of surveys: 

effort$season.yr.survey <- paste(effort$yr, effort$season, effort$survey.type)
effort <- effort[order(effort$season.yr.survey),]

uniq <- data.frame(table(effort$season.yr.survey))
colnames(uniq) <- c("Sampling Event","Number of Efforts")
uniq$Year <- substr(uniq[,1], 1,4)

uniq


#setwd(Db.connect)
#write.csv(uniq, "sampling events.csv", row.names=F)




### QA catch

# check for NFCs (efforts without corresponding catch) with additional catches
#nfcs <- catch.all[which(catch.all$species=="NFC"),"catchID"]
#catch.all[which(catch.all$catchID==nfcs),]
#length(nfcs)== nrow(catch.all[which(catch.all$catchID==nfcs),])
# If there are no errors, this should be "TRUE"

catch[which(catch$species=="LT"),30:36]
str(catch)





### QA LT.IDs 

LT <- LT.ID[(which(LT.ID$FishSpecies=="LT")),]
str(LT)

#check if any have dec tag, but no hex tag (preferred). Should be "integer(0)"
which(!is.na(LT$LTPitTag_DEC_ID_1)& is.na(LT$LTPitTag_HEX_ID_1))
which(!is.na(LT$LTPitTag_DEC_ID_2)& is.na(LT$LTPitTag_HEX_ID_2))
which(!is.na(LT$LTPitTag_DEC_ID_3)& is.na(LT$LTPitTag_HEX_ID_3))

#check if any have duplicate tag information (PIT tags)
#simple:
dup <- data.frame(table(LT$LTPitTag_HEX_ID_1))
dup[which(dup[,2]>1),1]

#complex:

LT$LTPitTag_HEX_ID_1 <- as.character(LT$LTPitTag_HEX_ID_1)
LT$LTPitTag_HEX_ID_2 <- as.character(LT$LTPitTag_HEX_ID_2)
LT$LTPitTag_HEX_ID_3 <- as.character(LT$LTPitTag_HEX_ID_3)

LT$tags <- paste(LT$LTPitTag_HEX_ID_1, LT$LTPitTag_HEX_ID_2, LT$LTPitTag_HEX_ID_3)

hex1 <- NA
length(hex1) <- nrow(LT)
for (i in 1:nrow(LT)){
  hex1[i] <- length(which(grepl(LT$LTPitTag_HEX_ID_1[i],LT$tags)== TRUE))
}
LT[which(hex1 > 1),c("LTFishIDAutonumber","tags")] 
#Fish # 642 + 503 not currently fixable, but pay attention to others

hex2 <- NA
length(hex2) <- nrow(LT)
for (i in 1:nrow(LT)){
  hex2[i] <- length(which(grepl(LT$LTPitTag_HEX_ID_2[i],LT$tags)== TRUE))
}
LT[which(hex2 > 1),c("LTFishIDAutonumber","tags")]


#check if any have duplicate tag information (floy tags)
#simple:
dup <- data.frame(table(LT$LTFLOYID1))
dup[which(dup[,2]>1),1]
#complex:

LT$LTFLOYID1 <- as.character(LT$LTFLOYID1)
LT$LTFLOYID2 <- as.character(LT$LTFLOYID2)
LT$LTFLOYID3 <- as.character(LT$LTFLOYID3)
LT$LTFLOYID4 <- as.character(LT$LTFLOYID4)

LT$floys <- paste(LT$LTFLOYID1, LT$LTFLOYID2, LT$LTFLOYID3, LT$LTFLOYID4)

floy1 <- NA
length(floy1) <- nrow(LT)
for (i in 1:nrow(LT)){
  floy1[i] <- length(which(grepl(LT$LTFLOYID1[i],LT$floys)== TRUE))
}
LT[which(floy1 > 1),c("LTFishIDAutonumber","floys")] 

floy2 <- NA
length(floy2) <- nrow(LT)
for (i in 1:nrow(LT)){
  floy2[i] <- length(which(grepl(LT$LTFLOYID2[i],LT$floys)== TRUE))
}
LT[which(floy2 > 1),c("LTFishIDAutonumber","floys")] 

floy3 <- NA
length(floy3) <- nrow(LT)
for (i in 1:nrow(LT)){
  floy3[i] <- length(which(grepl(LT$LTFLOYID3[i],LT$floys)== TRUE))
}
LT[which(floy3 > 1),c("LTFishIDAutonumber","floys")] 

floy4 <- NA
length(floy4) <- nrow(LT)
for (i in 1:nrow(LT)){
  floy4[i] <- length(which(grepl(LT$LTFLOYID4[i],LT$floys)== TRUE))
}
LT[which(floy4 > 1),c("LTFishIDAutonumber","floys")] 




# ** indiciate which fish are the unidentifiable re-caps
# Check out Recap Identifier field

#****don't use the Recap identifier for now
LT[,1:6]
levels(LT$LTRecapIdentifier)

LT[which(LT$LTRecapIdentifier=="Hatchery Cohort" & is.na(LT$LTCohortYr)),]
LT[which(!is.na(LT$LTRecapIdentifier)),c("LTFishIDAutonumber","LTRecapIdentifier","LTCohortYr","FishComments")]

levels(LT$LTRecapIdentifier)
#aborted- need to clean up this field and make more useful



#check which Lakers don't have a catch record

LT[which(LT$LTFishIDAutonumber %in% catch$LTFishID_Autonumber == FALSE),1:6]

#the problematic records appear to be 9000 to 11552. Not sure what the other ones were.. misfires?








#### 
#### TO DO
####

#**write script to give summary of captures of individual fish IDs 







# QA effort.catch

event <- levels(as.factor(effort.catch$season.yr.survey))

dupe <- effort.catch[1:length(event),]; 

i=3
effort.catch[which(duplicated(effort.catch[which(effort.catch$season.yr.survey == event[i]),
                                           c("LTFishID_Autonumber","species","FL","WT","count","sex")])==TRUE),]

effort.catch$qa.field1 <- paste(effort.catch$season.yr.survey,effort.catch$species,effort.catch$FL,
                                effort.catch$WT,effort.catch$sex)



#QA routines to write:
#write chunk for list of surveys that have occurred and how many efforts, 
#catch and by-catch they all have -> for review post-data entry
#write script for any LTs that have wt but no FL
#check if duplicate LT.ID'ing parameters
# translate x,y,z over from site ID





#for clean "source" run in other scripts:


rm("ch","bycatch.merge","catch.merge","env.merge","i","dup","dupe","floy1","floy2",
   "floy4","hex1","hex2","event","floy3","uniq")
ls()





































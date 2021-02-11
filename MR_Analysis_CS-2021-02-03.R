# modifications made by Carl Schwarz
#  1. Modified script to read Excel workbook created from Access database for my Macintosh

# This script is meant to analyze the mark-recapture data from Moberly Lake.
# Analysis originally written by B. Anderson
# Set-up and analysis modifications by K. Peck and R. Elsner

library(data.table)
library(FSA)
library(RMark)
library(tidyverse)
library(RODBC)


###
#### Run Moberly LT Database script ####
###


# make sure Db_connect.R below is saved before proceeding.
suppressWarnings(source("Db_connect_CS.R"))
ls()




##### Construct Capture Histories ####
#need to adjust for effort.catch

catch.history <- effortR %>% 
  dplyr::select(EffortAutoNumber,season,shoal,survey.type,gear.type) %>% 
  full_join(catchR) %>% 
  filter(species %in% "LT") %>% 
  mutate(freq = case_when(fate %in% c("a",NA) ~ 1,
                          fate %in% c("m","m?") ~ -1)) %>%   #assume that suspected deaths are real
  arrange(datetime)
xtabs(~yr+freq, data=catch.history, exclude=NULL, na.action=na.pass)

catch.history <- effort.catch %>% 
  filter(species %in% "LT") %>% 
  mutate(freq = case_when(fate %in% c("a",NA) ~ 1,
                          fate %in% c("m","m?") ~ -1)) %>%   #assume that suspected deaths are real
  arrange(datetime)
head(catch.history)

catch.history[ is.na(catch.history$LTFishIDAutonumber),]   ## CJS what does a missing LTAutonumber mean? No catch in a net sample?


# write.csv(catch.history, "catch.history.csv") 
# catch.history <- read.csv("catch.history.csv", row.names = NULL)
# headtail(catch.history)

# find the number of sexes in each fish to see if any fish are missing sex for all records
n.sex <- plyr::ddply(catch.history, c("LTFishIDAutonumber"), plyr::summarize,
                     n.sex =length(unique(sex)),
                     n.sex.all.missing = all(is.na(sex)))
n.sex[n.sex$n.sex > 1,]  # check that all fish have at most 1 sex
bad.sex <- n.sex[n.sex$n.sex.all.missing == 1,]  # fish missing sex in all records
bad.sex
catch.history$sex[ catch.history$LTFishIDAutonumber %in% bad.sex$LTFishIDAutonumber] <- 'un'  ## CJS how can a fish has missing sex and not 'un'?

#QA to check if fish were mis-recorded as dead and then found alive

qa.freq <- catch.history %>% 
  group_by(LTFishIDAutonumber) %>% 
  summarize(qa.freq = paste(freq, collapse = "" ), qa.sex = paste(sex,collapse = ""))
unique(qa.freq$qa.freq) #none of these should have a death in the middle of the series
unique(qa.freq$qa.sex) #none of these should switch sex in the middle of the series



#some fixes caught by this QA:
# 
#fixed 143 (was a suspected death from poor release in 2013, but later recaptured)
#fixed 219, where the date of lethal sample was mis-recorded
#fixed 12274 (581) -> this fish (caught Aug 22, 2017) should have been fish #341.


#catch history per year, all seasons, all types:
(catch.hist.byyr <- catch.history %>%  
    group_by(LTFishIDAutonumber, yr) %>% 
    dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>%   ## CJS add dplyr:: 
    arrange(LTFishIDAutonumber))


#df of catches by year and season, all types:
(catch.hist.byseasonyr <- catch.history %>%  
    filter(season %in% c("spring", "summer", "fall")) %>% 
    mutate(season.num = case_when(season %in% "spring" ~ "1spring",
                                  season %in% "summer" ~ "2summer",
                                  season %in% "fall" ~ "3fall")) %>% 
    mutate(season.yr = factor(paste0(yr,"-",season.num), ordered=T)) %>% 
    group_by(LTFishIDAutonumber, season.yr) %>% 
    dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>%  ## CJS add dplyr::
    mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
    arrange(LTFishIDAutonumber)) 


  #capture histories from all seasons, all recap types:
(ch.allsampling <- spread(data=catch.hist.byseasonyr, key=season.yr, value = tot.catches,
                          fill=0))

cols <- names(ch.allsampling)[4:ncol(ch.allsampling)]
ch.allsampling$ch <- do.call(paste, c(ch.allsampling[cols],sep=""))
headtail(ch.allsampling)



#df and capture histories on spawning shoals only (excluding holding pen):
catch.hist.spawner <- catch.history %>%  
  filter(survey.type %in% "Spawner Sampling/Tagging", 
         gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                          "Seine Net", "Angling")) %>% 
  group_by(LTFishIDAutonumber, yr) %>% 
  dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>%  ## CJS added dplyr::
  mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
  arrange(LTFishIDAutonumber, yr) 
catch.hist.spawner

## CJS YOu want to remove the 2005, 2006, 2007 data and start with 2008 as the
##     number of captures in the first 3 years is very small
plyr::ddply(catch.hist.spawner, "yr", plyr::summarize, n.fish=length(yr))
catch.hist.spawner <- catch.hist.spawner[ catch.hist.spawner$yr >= 2008,]


ch.spawner <- catch.hist.spawner %>% 
  spread(key=yr, value = tot.catches,fill=0) 

cols <- names(ch.spawner)[4:ncol(ch.spawner)]
ch.spawner$ch <- do.call(paste, c(ch.spawner[cols],sep=""))
headtail(ch.spawner)
length(cols) #this is the number of events

#see if any fish in the dataset have no captures since these will not work in MR analysis
which(ch.spawner$ch == paste(rep(0,length(4:ncol(ch.spawner))-1),collapse =""))


# ### older script: ####
# effort.catch$season.yr <- ifelse(effort.catch$survey.type=="Spawner Sampling/Tagging"|effort.catch$survey.type=="Spawner Sampling",
# 	paste0(substr(effort.catch$season,1,8),effort.catch$yr),NA)
# 
# events.df <-unique(effort.catch[which(effort.catch$survey.type=="Spawner Sampling/Tagging"|effort.catch$survey.type=="Spawner Sampling"),
# 	c("yr","season","survey.type")])
# events.df <- events.df[order(events.df$yr),]
# 
# # events.df$season.yr <- paste0(substr(events.df$season,1,8),events.df$yr)
# # 
# # #events <- events.df$season.yr # all years since 2005
# # (events <- events.df$season.yr[c(0,4:14)]) # all years since 2008
# # 
# # str(effort.catch)
# # catchR <- effort.catch[which(effort.catch$species=="LT"),]
# # 
# # #take out any LT that were lost from the nets (and therefore have no ID)
# # #### catchR <- catchR[which(!is.na(catchR$LTFishID_Autonumber)),]
# 
# 
# #Query: for interest, generate a table with how many recaps per individual and the max recaps
# recaps <- data.frame(table(catchR$LTFishIDAutonumber));colnames(recaps) <- c("LT auto ID","times recapped")
# (recaps[which(recaps[,2]==max(recaps[,2])),])
# (props <- xtabs(~recaps$`times recapped`))
#  
# 
# #### Capture Histories #
# #make capture histories for all fish, but only the events for fall will be identified
# caphist <- data.frame(MSAccess_Fish_ID=catchR$LTFishIDAutonumber)
# caphist$sex <- catchR$sex
# caphist$yr.class <- catchR$yr.class
# caphist$event <- catchR$season.yr
# 
# ch$freq <- as.character(catchR$fate)
# ch <- ch %>%  mutate(freq = as.numeric(ifelse(freq=="m",-1, # assumes morts -1
#                       ifelse(!freq=="m?"|freq=="a",1,NA)))) %>% # if unsure of mort then assumed alive
#   arrange(MSAccess_Fish_ID,freq) # arrange by fishID and so that mort shows up first where fish are captured multiple times
# ch$freq <- ifelse(is.na(ch$freq),"1",ch$freq)
# 
# # below ensures that all mort fish are coded as -1 and all living fish are +1
# for (i in 2:nrow(ch)){
#   ch$freq[i] <- ifelse(ch$MSAccess_Fish_ID[i]==ch$MSAccess_Fish_ID[i-1],ch$freq[i-1],
#                          ifelse(ch$MSAccess_Fish_ID[i]!=ch$MSAccess_Fish_ID[i-1],ch$freq[i],0))
# }
# ch$freq <- as.numeric(ch$freq)
# 
# #add events
# for (i in 1:length(events)){
# 	ch[,events[i]] <- 0
# 	}
# for (i in 1:length(events)){
# 	ch[,events[i]] <- ifelse(ch$event==events[i],1,0)
# 	}
# ch <- ch[order(ch$MSAccess_Fish_ID),]
# 
# #only keep MALES caught during FALL spawner events
# ch <- ch[which(!is.na(ch$event)&ch$sex=="m"),]
# 
# ch.sum <- data.frame(MSAccess_Fish_ID = unique(ch$MSAccess_Fish_ID))
# for (i in 1:length(events)){
# 	ch.sum[,events[i]] <- 0
# 	}
# 
# for (i in 1:length(events)){
# 	ch.sum[,events[i]] <- tapply(ch[,events[i]],ch$MSAccess_Fish_ID,FUN=sum)
# 	}
# 
# ch.sum$freq <- tapply(ch$freq,ch$MSAccess_Fish_ID, FUN=prod)
# 
# ## export to a capture histories file (if you want)
# #setwd(outputs2017)
# #write.csv(ch.sum, "capture_histories.csv", row.names=F)
# 
# #for fish that were captured more than once in the capture event, just give them a "1"
# for (i in 1:length(events)){
# ch.sum[which(ch.sum[,(events[i])] > 1),events[i]] <- 1
# }
# 
# ch.sum$ch <- apply(ch.sum[,2:12],1,paste0, collapse="") # CHANGES MADE HERE
# 
# # calc recap percentage vs fish never recapped
# 
# recapped <- ch.sum[which(ch.sum$ch!="00000000000"),] # CHANGES MADE HERE
# nonrecapped <- ch.sum[which(ch.sum$ch=="00000000000"),] # CHANGES MADE HERE
# 
# ##remove males with no fall captures
# ch.sum <- ch.sum[which(ch.sum$ch!="00000000000"),] # CHANGES MADE HERE
# head(ch.sum)
# # 
# 



####
##### Mark-recap Analysis - POPAN #### 
#*** Make sure that you have Mark downloaded onto your computer from here: 
# http://www.phidot.org/software/mark/.  

## The following lines are based on B. Anderson's scripts and have not recently been updated.

#establish the process.data for analysis
####time intervals are fractions of a year between capture events, and includes the length of time elapsed since the last capture event -- must be updated to current time
####nocc is the number of capture occasions
####begin.time is set to 2008 to indicate the fall period of that year
#and create the default design data - ddl

#select only male spawners
ch.spawnerm <- ch.spawner %>% 
  filter(sex %in% "m")
headtail(ch.spawnerm)


#spawner.model <- mark(ch.spawner, model="POPAN", begin.time = 2008) 





## Brendan - this is where I stopped... KP

## CJS. Note that RMark gets upset with tibbles
ch.spawnerm <- as.data.frame(ch.spawnerm)

moberly.proc = process.data(ch.spawnerm, model= "POPAN", begin.time=2008)
(moberly.ddl=make.design.data(moberly.proc))


#list the elements of the ddl dataframe
mode(moberly.ddl)
names(moberly.ddl)

#### POPAN Parameters ####
#to set up the possible parameters for later selection/inclusion within models
Phi.dot=list(formula=~1)  #constant survival
Phi.Time=list(formula=~Time)  #survival varying by time as a trend
Phi.time=list(formula=~time)  #survival varying by time but not as a trend

p.dot=list(formula=~1) # capture probability constant
## CJS. Given the way that RMARK parameterizes all parameters, it will sometimes be sensible to use
##      a parameterization that models each parameter as individual parameters rather than as offset
##      from the first parameter. This is especially true when the first parameter (e.g. p(1) is weird, in this case
##      very little effort at first capture occasion.)
##      Results will be identical under this or the simple ~time parameterization but can be much more numerically stable
p.time   =list(formula=~-1+time)  #capture probability varies with event
p.time.cs=list(formula=~-1+time,fixed=list(time=2008,value=1))  #capture probability varies with event, initial p value set to 1 as it cannot be estimated... based on advise from Carl Schwarz

#pent.time=list(formula=~time)  #probability of entrance from the superpopulation varies with event... variable recruitment
## CJS.. The only sensible models for pent are t or perhaps . Does not make sense for a T model 
## CJS   This is because the pents must sum to zero 
pent.dot=list(formula=~1)    #probability of entrance from the superpopulation is constant...not considered as this requires that initial population size be equivalent to annual immigration thereafter 
#pent.Time=list(formula=~Time)    #probability of entrance from the superpopulation varies with time as a Trend

## CJS   I assume here you want a zero recruitment model. This formula doesn't work (look at estimates when the model is fit)
pent.zero   =list(formula=~1, fixed=~0)  #sets probablity of entrance to zero (zero recruitment)... added this in 2015 but still need to run it
## CJS   Note that pents are indexed by the END of the interval, i.e the pent for 2009 is the pent between 2008 and 2009 (sigh!).
pent.zero.cs=list(formula=~1, fixed=list(time=2009:2020,value=0)) 
pent.time   =list(formula=~time)   ## CJS



#run models with selected parameters of interest from above for S (=Phi), p and pent
#first six models represent the possible combinations of S and pent, with p varying by event
#difficulty pointed out with beta parameters by CS, seems to be an issue of confounding in first p, in fully time-varying models

#CS points out that in popan models with p(t), the first p is NEVER estimable... 
#so consider including his fix i all models even though it only seems to create problematic outputs in models 2 and 5   

#### POPAN models ####
#not clear how to sort this out and CS's recommended fix doesn't seem to do it... 
#the red notes in output indicating "Note: only xx parametes counted of xx specified" seem to 
#corresponed to the instances of problematic  beta parameter outputs 
#ultimately, I have elected to only include models that are not fully time dependant 
#(i.e. Phi and pent are either constant or varying as a trend, but not ~time) 
##CJS don't fit a pent(T) models as it really doesn't make sense. 
#model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))
#model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.Time))
#model.3=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero))

model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))

## CJS check total number of captures by year
colSums(ch.spawnerm[, as.character(2008:2020)])


## CJS It often helps to use the initial values from a simpler model when fitting a slightly more complex model
##     The message for model.2 is because some of the pent beta are highly negative with huge standard errors 
##     indicating that they are essentially zero. (logit(0)=-infinity on beta scale)
##     RMarks has trouble dealing with the very negative parameter.
#model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.Time))  ## CJS pent.Time doesn't make sense
model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time.cs, pent=pent.time), initial=model.1)

## CJS I assume you want to test if no recritment is happening, i.e. a death only model. Your pent.zero didn't work
##     THe model.3 gives NA for the quick results for pent, but if you look at the long real results everything is ok
##     This is because all parameters were fixed to 0. 
model.3.old=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero))
model.3.old$results$real
rm(model.3.old)

## CJS here is the proper model for no recruitment
model.3    =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero.cs))
model.3$results$real


model.4=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.dot))

#model.5=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.Time)) ## CJS pent.Time doesn't make sense
## CJS notice that model 5 fits fine even though pent(1)p(1) are confounded. The beta parameter estimates show p(200) having
##     a huge standard error indicating that it is confound.
##     Better to refit with my correction
## CJS do you really want a linear trend in Phi?
model.5.old=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.time))
summary(model.5.old)
rm(model.5.old)

model.5=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time.cs, pent=pent.time))  ## CJS better parameterization
model.5$results$real


## CJS Again, I assume you want a no recruitment model
model.6.old=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.zero)) ## doesn't work properly
model.6.old$results$real
rm(model.6.old)
## CS here is the proper death only model. Again note that simple summary for pents shows NAs but longer list looks ok
model.6=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.zero.cs))
model.6$results$real

## CJS seems to work with my revisions but seem to be the same as model 2?
##     Warning about parameter counts is because pents are very small (betas are -infinity)
#model.7=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time.cs, pent=pent.time)) model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time.cs, pent=pent.time), initial=model.1)


#### POPAN model evaluation ####
#collect the results of all the models and compare
#smallest AIC value is most parsimonious
#rule of thumb is that a 'delta AIC' of <2 suggests substantial evidence for a model, while delta AICs values of 3-7 indicate considerably less support, >10 indicates that teh model is very unlikely
(moberly.JS.results=collect.models(type="POPAN"))

## CJS before you accept the model table, you need to check the parameter counts.
##     especially when you have a pent(time) model and some of the parameters are going to zero (and the betas are going to -Inf)
##     For example, look at model 5. THe "red" message says that it sees 23 rather than 27 parameters because 4 pents are -Inf,
##     but in the AIC table it counts it anyways.
##     HEre are the true parameter counts
##       Model 1  1  (phi      ) + 13 (p) +  1 (pent) + 1 (N) = 16
##       Model 5  2  (phi trend) + 13 (p) + 12 (pent) + 1 (N) = 28 - p(1)pent(1) confound = 27
##       Model 4  2  (phi trend) + 13 (p) +  1 (pent) + 1 (N) = 17
##       Model 2  1  (phi      ) + 13 (p) + 12 (pent) + 1 (N) = 27  - p(1)pent(1) founded = 26
##     Rest of models have no support and so I didn't look in detail about them 

rm(model.6, model.3)

# top two models averaged
(moberly.JS.results=collect.models(type="POPAN"))

#note that 'moberly.JS.results' has a particular structure which is of the class 'marklist'
#a marklist contains a list element for each model that was run
mode(moberly.JS.results)
class(moberly.JS.results)
names(moberly.JS.results)

#### POPAN results ####
#to see the real results with confidence limits for any model
model.1$results$real      # or could use... summary(moberly.JS.results[[2]])
model.2$results$real
model.4$results$real
model.5$results$real



############################################################
#now average the results of the models using the AIC weighting
(moberly.JS.mod.avg=model.average(moberly.JS.results,vcv=TRUE))
names(moberly.JS.mod.avg)
popan.ests <- moberly.JS.mod.avg$estimates

## CJS. The model averaging below gives a warninga about a problem with the VCV for some 
##      models. THis is a result of pent parameters going to 0 in some models. 
##      Set the drop=FALSE term to avoid dropping model
(Phi.ests <- model.average(moberly.JS.results, "Phi")) #, vcv=TRUE

## CJS compare the two following model averaging. THe first one below drops one of the models, but you 
##     don't have to do that.
(pent.ests <- model.average(moberly.JS.results, "pent")) #, vcv=TRUE
(pent.ests <- model.average(moberly.JS.results, "pent", drop=FALSE)) #, vcv=TRUE

(p.ests <- model.average(moberly.JS.results, "p")) #, vcv=TRUE

#the support notes for Package 'RMark' provide background on the 'popan.derived' function on p 127
#it indicates that if a 'marklist' is provided (such as 'moberly.JS.results') the results are model-averaged
class(moberly.JS.results)

(derived <- popan.derived(moberly.proc, moberly.JS.results, N=TRUE))
names(derived)
Nbyocc <- derived$Nbyocc


######################################################################

#### Mark-recap Analysis Pradel ####

## CJS. The Pradel models are just a different parameterization of the JS model
##      THe key advantage is the ability to get lambda (population) growth directly
##      without having to do any computations.
##      See my article in the Gentle Introduction to MARK where I go through the various parameterization.

#establish the process.data for analysis
####time intervals are fractions of a year between capture events, and includes the length of time elapsed since the last capture event -- must be updated to current time
####nocc is the number of capture occasions
####begin.time is set to 2008 to indicate the fall period of that year
#and create the default design data - ddl

## CJS  Not necessary specify time interval if all equal
moberly.proc = process.data(ch.spawnerm, model= "Pradlambda", begin.time=2008)

moberly.ddl=make.design.data(moberly.proc)


#list the elements of the ddl dataframe
mode(moberly.ddl)
names(moberly.ddl)
moberly.ddl$Lambda$Time2 <- (moberly.ddl$Lambda$Time-5)^2
moberly.ddl$Lambda

#### Pradel Parameters ####

## CJS only those that differ from the POPAN parameterization are shown below
lambda.dot  =list(formula=~1)    #population growth rate is constant 
lambda.Time =list(formula=~Time)    #populations growth rate varies as a trend with time
lambda.Time2=list(formula=~Time+Time2)    #populations growth rate varies as a trend with time
lambda.time =list(formula=~time)    #populations growth rate varies over time without a trend


#### Pradel models ####
#run models with selected parameters of interest from above for S (=Phi), p and lambda
#seems to be no confounding of parameters

## CJS All seem to run properly
pmodel.1  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.dot))
pmodel.2  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.Time))
pmodel.3  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.time))
pmodel.4  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.dot))
pmodel.5  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.Time))
pmodel.6  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.dot))
pmodel.7  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.time, p=p.time, Lambda=lambda.dot))
pmodel.8  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.time))
pmodel.9  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.time, p=p.time, Lambda=lambda.time))
pmodel.10 =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot , p=p.dot,  Lambda=lambda.Time))
pmodel.11  =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.Time2))


#collect the results of all the models and compare
#smallest AIC value is most parsimonious
#rule of thumb is that a 'delta AIC' of <2 suggests substantial evidence for a model, while delta AICs values of 3-7 indicate considerably less support, >10 indicates that teh model is very unlikely
moberly.pradel.results=collect.models(type="Pradlambda")
moberly.pradel.results # model.2 and model.4 supported need to average below and remove other two models

##
#to see the real results with confidence limits for any model
pmodel.1$results$real      # or could use... summary(moberly.pradel.results[[2]])
pmodel.2$results$real
pmodel.3$results$real
pmodel.4$results$real

rm(pmodel.1,pmodel.3) # removed model.1 and model.3 because they were >2 AIC scores compared to the top model

##
#### Pradel model evaluation ####
#now average the results of the models using the AIC weighting
(moberly.pradel.mod.avg=model.average(moberly.pradel.results,vcv=TRUE))
names(moberly.pradel.mod.avg)
pradel.ests <- moberly.pradel.mod.avg$estimates


#### Pradel model results ####

Phi.ests <-    model.average(moberly.pradel.results, "Phi") #, vcv=TRUE
p.ests   <-    model.average(moberly.pradel.results, "p") #, vcv=TRUE
Lambda.ests <- model.average(moberly.pradel.results, "Lambda") #, vcv=TRUE

# save the model averaged results for extraction in power analysis program
save(Phi.ests,    file="moberly.pradel.ma.Phi.RData")
save(p.ests     , file="moberly.pradel.ma.p.RData")
save(Lambda.ests, file="moberly.pradel.ma.Lambda.RData")

# for plotting
Lambda.ests <- cbind(Lambda.ests, moberly.pradel.mod.avg$estimates[moberly.pradel.mod.avg$estimates$par.index %in% Lambda.ests$par.index,4:5])

library(ggplot2)
ggplot(Lambda.ests, aes(x=time, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1) +   ## CJS I like narrower error bars
  scale_y_continuous(breaks=seq(0.5,1.75, 0.25), limits=c(0.5,1.75)) +
  xlab(label = "Year") + ylab(label="Lambda estimate (95% ci)") +   ## CJS indicate that bars are 95% ci
  geom_hline(yintercept=1, linetype="dashed" ) +
  theme_classic()

######################################################################
cleanup(ask=FALSE)

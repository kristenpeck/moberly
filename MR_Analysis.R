

# This script is meant to analyze the mark-recapture data from Moberly Lake.
# Analysis originally written by B. Anderson
# Set-up and analysis modifications by K. Peck and R. Elsner

library(data.table)
library(FSA)
library(RMark)
library(RODBC)


###
#### Run Moberly LT Database script ####
###


# make sure Db_connect.R below is saved before proceeding.
source("Db_connect.R")
ls()


##### Construct Capture Histories ####

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
unique(qa.freq$qa.freq) #none of these should have a death in the middle of the series
unique(qa.freq$qa.sex) #none of these should switch sex in the middle of the series

# qa.freq[which(qa.freq$qa.freq %in% c("-11", "11-11111")),]
# catch.history %>% 
#   filter(LTFishIDAutonumber %in% c(143, 219 , 12274)) %>% 
#   arrange(LTFishIDAutonumber, datetime)
# 
#fixed 143 (was a suspected death from poor release in 2013, but recaptured)
#fixed 219, where the date of lethal sample was mis-recorded
#fixed 12274 (581) -> this fish (caught Aug 22, 2017) should have been fish #341. Changed

#catches per year, all seasons, all types:
(catch.hist.byyr <- catch.history %>%  
    group_by(LTFishIDAutonumber, yr) %>% 
    summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
    arrange(LTFishIDAutonumber))


#df of catches by year and season, all types:
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
  summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
  mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
  arrange(LTFishIDAutonumber, yr) 
catch.hist.spawner

ch.spawner <- catch.hist.spawner %>% 
  spread(key=yr, value = tot.catches,fill=0) 

cols <- names(ch.spawner)[4:ncol(ch.spawner)]
ch.spawner$ch <- do.call(paste, c(ch.spawner[cols],sep=""))
headtail(ch.spawner)


#see if any fish in the dataset have no captures
no.catch <- paste(rep(0,length(4:ncol(ch.spawner))-1),collapse ="")
which(ch.spawner$ch == no.catch)


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

## RE:
#The data should be all set up and you should be able to mess around with the variables. 
#These are BA's notes, CS refers to Carl Schwarts who is a stats guy he was talking to. 
# Read the package manual and that coil-bound guide that I left in the cabinet. Good luck!!!  
######################################

#establish the process.data for analysis
####time intervals are fractions of a year between capture events, and includes the length of time elapsed since the last capture event -- must be updated to current time
####nocc is the number of capture occasions
####begin.time is set to 2008 to indicate the fall period of that year
#and create the default design data - ddl

ch.spawnerm <- ch.spawner %>% 
  filter(sex %in% "m")
headtail(ch.spawnerm)

moberly.proc = process.data(ch.spawnerm, model= "POPAN", begin.time=2008, nocc=10, 
                            time.intervals=c(1,1,1,1,1,1,1,1,1,1))
(moberly.ddl=make.design.data(moberly.proc))


#list the elements of the ddl dataframe
mode(moberly.ddl)
names(moberly.ddl)

#### POPAN Parameters ####
#to set up the possible parameters for later selection/inclusion within models
Phi.dot=list(formula=~1)  #constant survival
Phi.Time=list(formula=~Time)  #survival varying by time as a trend

p.dot=list(formula=~1) # capture probability constant
p.time=list(formula=~time)  #capture probability varies with event
p.time.cs=list(formula=~time,fixed=list(time=2008,value=1))  #capture probability varies with event, initial p value set to 1 as it cannot be estimated... based on advise from Carl Schwarz

#pent.time=list(formula=~time)  #probability of entrance from the superpopulation varies with event... variable recruitment
pent.dot=list(formula=~1)    #probability of entrance from the superpopulation is constant...not considered as this requires that initial population size be equivalent to annual immigration thereafter 
pent.Time=list(formula=~Time)    #probability of entrance from the superpopulation varies with time as a Trend
pent.zero=list(formula=~1, fixed=~0)  #sets probablity of entrance to zero (zero recruitment)... added this in 2015 but still need to run it

#run models with selected parameters of interest from above for S (=Phi), p and pent
#first six models represent the possible combinations of S and pent, with p varying by event
#difficulty pointed out with beta parameters by CS, seems to be an issue of confounding in first p, in fully time-varying models

#CS points out that in popan models with p(t), teh first p is NEVER estimable... so consider including his fix i all models even though it only seems to create problematic outputs in models 2 and 5   

#### POPAN models ####
#not clear how to sort this out and CS's recommended fix doesn't seem to do it... 
#the red notes in output indicating "Note: only xx parametes counted of xx specified" seem to corresponed to the instances of problematic  beta parameter outputs 
#ultimately, I have elected to only include models that are not fully time dependant (i.e. Phi and pent are either constant or varying as a trend, but not ~time) 
#model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))
#model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.Time))
#model.3=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero))

model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))
model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.Time))
model.3=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero))
model.4=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.dot))
model.5=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.Time))
model.6=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, pent=pent.zero))

#model.71cs=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time.cs, pent=pent.time))

#model.81a=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=list(formula=~time,fixed=list(time=2008,value=1) ), pent=pent.time))
#rm(model.1,model.2,model.3,model.4,model.5,model.6,model.71cs,model.81a)

#models with parameter warnings in red are #71cs and 81a
#red warning occurs in these models with or without carls modification, but results look fine when including carls modification

#### POPAN model evaluation ####
#collect the results of all the models and compare
#smallest AIC value is most parsimonious
#rule of thumb is that a 'delta AIC' of <2 suggests substantial evidence for a model, while delta AICs values of 3-7 indicate considerably less support, >10 indicates that teh model is very unlikely
(moberly.JS.results=collect.models(type="POPAN"))

rm(model.1,model.3,model.4,model.6)

# top two models averaged
(moberly.JS.results=collect.models(type="POPAN"))

#note that 'moberly.JS.results' has a particular structure which is of teh class 'marklist'
#a marklist contains a list element for each model that was run
mode(moberly.JS.results)
class(moberly.JS.results)
names(moberly.JS.results)

#### POPAN results ####
#to see the real results with confidence limits for any model
model.1$results$real      # or could use... summary(moberly.JS.results[[2]])
model.2$results$real
model.3$results$real
#model.4$results$real
#model.5$results$real
#model.6$results$real

#model.7$results$real  #this is model 2 but without cs addtion, seems to produce confounded results
#model.8$results$real  #this is model 5 but without cs addtion,seems to produce confounded results
#model.9$results$real  #this is model 4 but with cs addtion, model 4 was ok and this model has less AIC weight... no improvement 


############################################################
#now average the results of the models using the AIC weighting
(moberly.JS.mod.avg=model.average(moberly.JS.results,vcv=TRUE))
names(moberly.JS.mod.avg)
popan.ests <- moberly.JS.mod.avg$estimates

(Phi.ests <- model.average(moberly.JS.results, "Phi")) #, vcv=TRUE
(pent.ests <- model.average(moberly.JS.results, "pent")) #, vcv=TRUE
(p.ests <- model.average(moberly.JS.results, "p")) #, vcv=TRUE

#the support notes for Package 'RMark' provide background on the 'popan.derived' function on p 127
#it indicates that if a 'marklist' is provided (such as 'moberly.JS.results') the results are model-averaged
class(moberly.JS.results)

(derived <- popan.derived(moberly.proc, moberly.JS.results, N=TRUE))
names(derived)
Nbyocc <- derived$Nbyocc


######################################################################

#### Mark-recap Analysis Pradel ####

#establish the process.data for analysis
####time intervals are fractions of a year between capture events, and includes the length of time elapsed since the last capture event -- must be updated to current time
####nocc is the number of capture occasions
####begin.time is set to 2008 to indicate the fall period of that year
#and create the default design data - ddl

moberly.proc = process.data(ch.sum, model= "Pradlambda", begin.time=2008, nocc=10, time.intervals=c(1,1,1,1,1,1,1,1,1,1))
moberly.ddl=make.design.data(moberly.proc)


#list the elements of the ddl dataframe
mode(moberly.ddl)
names(moberly.ddl)

#### Pradel Parameters ####

#to set up the possible parameters for later selection/inclusion within models
Phi.dot=list(formula=~1)  #constant survival
Phi.Time=list(formula=~Time)  #survival varying by time as a trend

p.time=list(formula=~time)  #capture probability varies with event
#p.time.cs=list(formula=~time,fixed=list(time=2005,value=1))  #capture probability varies with event, initial p value set to 1 as it cannot be estimated... based on advise from Carl Schwarz

lambda.dot=list(formula=~1)    #population growth rate is constant 
lambda.Time=list(formula=~Time)    #populations growth rate varies as a trend with time


#### Pradel models ####
#run models with selected parameters of interest from above for S (=Phi), p and lambda
#seems to be no confounding of parameters

model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, Lambda=lambda.dot))
model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, Lambda=lambda.Time))
model.3=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.dot))
model.4=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.Time))


#collect the results of all the models and compare
#smallest AIC value is most parsimonious
#rule of thumb is that a 'delta AIC' of <2 suggests substantial evidence for a model, while delta AICs values of 3-7 indicate considerably less support, >10 indicates that teh model is very unlikely
moberly.pradel.results=collect.models(type="Pradlambda")
moberly.pradel.results # model.2 and model.4 supported need to average below and remove other two models

#note that 'moberly.pradel.results' has a particular structure which is of the class 'marklist'
#a marklist contains a list element for each model that was run
mode(moberly.pradel.results)
class(moberly.pradel.results)
names(moberly.pradel.results)

##
#to see the real results with confidence limits for any model
model.1$results$real      # or could use... summary(moberly.pradel.results[[2]])
model.2$results$real
model.3$results$real
model.4$results$real

rm(model.1,model.3) # removed model.1 and model.3 because they were >2 AIC scores compared to the top model

##
#### Pradel model evaluation ####
#now average the results of the models using the AIC weighting
(moberly.pradel.mod.avg=model.average(moberly.pradel.results,vcv=TRUE))
names(moberly.pradel.mod.avg)
pradel.ests <- moberly.pradel.mod.avg$estimates

#### Pradel model results ####
#the support notes for Package 'RMark' provide background on the 'popan.derived' function on p 127
#it indicates that if a 'marklist' is provided (such as 'moberly.JS.results') the results are model-averaged
#look at one parameter at a time

(Phi.ests <- model.average(moberly.pradel.results, "Phi")) #, vcv=TRUE
(p.ests <- model.average(moberly.pradel.results, "p")) #, vcv=TRUE
(lambda.ests <- model.average(moberly.pradel.results, "Lambda")) #, vcv=TRUE

# for plotting
lambda.ests <- cbind(lambda.ests, moberly.pradel.mod.avg$estimates[22:31,4:5])

library(ggplot2)
ggplot(lambda.ests, aes(x=time, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl, ymax=ucl)) +
  scale_y_continuous(breaks=seq(0.5,1.75, 0.25), limits=c(0.5,1.75)) +
  xlab(label = "Year") + ylab(label="Lambda estimate") +
  geom_hline(yintercept=1, linetype="dashed" ) +
  theme_classic()

######################################################################
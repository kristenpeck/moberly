

# This script is meant to analyze the mark-recapture data from Moberly Lake.
# Analysis originally written by B. Anderson
# Set-up and analysis modifications by K. Peck and R. Elsner
# Comments and analysis adopted from C. Schwarz in 2021

library(data.table)
library(FSA)
library(RMark)
library(tidyverse)
library(RODBC)

# citation("FSA")
# citation("RMark")

###
#### Run Moberly LT Database script ####
###


# make sure Db_connect.R below is saved before proceeding.
source("Db_connect.R")
ls()


##### Construct Capture Histories ####
nrow(effort.catch %>% 
  filter(fate %in% "m?")) #there are currently (as of 2021) few fish with a fate of "m?"


catch.history <- effort.catch %>% 
  filter(species %in% "LT") %>% 
  filter(!is.na(LTFishIDAutonumber)) %>% # took out NAs up here
  mutate(freq = case_when(fate %in% c("a",NA) ~ 1,
                          fate %in% c("m","m?") ~ -1)) %>%   #assume that suspected deaths are real
  arrange(datetime)

headtail(catch.history)

#catch.history[ is.na(catch.history$LTFishIDAutonumber),]   ## CJS what does a missing LTAutonumber mean? No catch in a net sample?
# KP: missing LTFishIDAutonumber usually means that we didn't actually get the fish 
#   into the boat, so these fish would be counted in CPUE but we don't know who they were.
#   Likely can't do much with them for MR...


  #optional: can export this df if sending to someone, and they can reload from here bu uncommenting:
  # write.csv(catch.history, "catch.history.csv") 
  # catch.history <- read.csv("catch.history.csv", row.names = NULL)



#QA to check if fish were mis-recorded as dead and then found alive, or switched sex
qa.freq <- catch.history %>% 
  dplyr::group_by(LTFishIDAutonumber) %>% 
  dplyr::summarize(qa.freq = paste(freq, collapse = ""), qa.sex = paste(sex,collapse = ""))
unique(qa.freq$qa.freq) #none of these should have a death in the middle of the series
unique(qa.freq$qa.sex) #none of these should switch sex in the middle of the series

#some fixes caught by this QA:
# 
#fixed 143 (was a suspected death from poor release in 2013, but later recaptured)
#fixed 219, where the date of lethal sample was mis-recorded
#fixed 12274 (581) -> this fish (caught Aug 22, 2017) should have been fish #341.


#catch history per year, all seasons, all types:
(catch.hist.byyr <- catch.history %>%  
    dplyr::group_by(LTFishIDAutonumber, yr) %>% 
    dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
    arrange(LTFishIDAutonumber))


#df of catches by year and season, all types:
(catch.hist.byseasonyr <- catch.history %>%  
    filter(season %in% c("spring", "summer", "fall")) %>% 
    mutate(season.num = case_when(season %in% "spring" ~ "1spring",
                                  season %in% "summer" ~ "2summer",
                                  season %in% "fall" ~ "3fall")) %>% 
    mutate(season.yr = factor(paste0(yr,"-",season.num), ordered=T)) %>% 
    group_by(LTFishIDAutonumber, season.yr) %>% 
    dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
    mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
    arrange(LTFishIDAutonumber)) 


  #capture histories from all seasons, all recap types:
(ch.allsampling <- spread(data=catch.hist.byseasonyr, key=season.yr, value = tot.catches,
                          fill=0))

cols <- names(ch.allsampling)[4:ncol(ch.allsampling)]
ch.allsampling$ch <- do.call(paste, c(ch.allsampling[cols],sep=""))
headtail(ch.allsampling)





#### Fidelity to shoals #### 


# is a single fish captured on more than one shoal in a year? Only 8 cases as of 2021...

n.shoals <- catch.history %>% 
  filter(survey.type %in% "Spawner Sampling/Tagging", 
         gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                          "Seine Net", "Angling")) %>% 
  dplyr::group_by(LTFishIDAutonumber, yr,sex) %>% 
  filter(!is.na(LTFishIDAutonumber)) %>% 
  dplyr::summarize(n.shoals = length(unique(shoal)),shoals = paste(unique(shoal), collapse=""))


# The fish where multiple shoals were visited in one yr.
n.shoals[ n.shoals$n.shoals > 1,] 



#df and capture histories on spawning shoals only (excluding holding pen):
catch.hist.spawner <- catch.history %>%  
  filter(survey.type %in% "Spawner Sampling/Tagging", 
         gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                          "Seine Net", "Angling")) %>% 
  dplyr::group_by(LTFishIDAutonumber, yr) %>% 
  dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count),
                   shoal=unique(shoal)[1]) %>%  #takes first shoal within year 
  mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
  arrange(LTFishIDAutonumber, yr) 
catch.hist.spawner


# Totals caught at shoals each year
xtabs(~yr+shoal, data=catch.hist.spawner, exclude=NULL, na.action=na.pass)


## CJS You want to remove the 2005, 2006, 2007 data and start with 2008 as the
##     number of captures in the first 3 years is very small
plyr::ddply(catch.hist.spawner, "yr", plyr::summarize, n.fish=length(yr))

catch.hist.spawner <- catch.hist.spawner[ catch.hist.spawner$yr >= 2008,]

#construct catch histories
ch.spawner <- catch.hist.spawner %>%  #658 x 18
  spread(key=yr, value = tot.catches,fill=0) 

cols <- names(ch.spawner)[5:ncol(ch.spawner)]
ch.spawner$ch <- do.call(paste, c(ch.spawner[cols],sep=""))
headtail(ch.spawner)
length(cols) #this is the number of events

#QA-see if any fish in the dataset have no captures since these will not work in MR analysis
which(ch.spawner$ch == paste(rep(0,length(5:ncol(ch.spawner))-1),collapse =""))

#same thing, newer script #scratch that-seems there are a different number of rows? 492x16 to investigate
ch.spawner       <- catch.hist.spawner %>%  
  pivot_wider(id_cols=c("LTFishIDAutonumber","sex"), names_from=yr, values_from=tot.catches, 
              values_fill=0, names_sort=TRUE)
head(ch.spawner)

cols <- names(ch.spawner)[-(1:2)]
cols

# check assumption of free movement between shoals:

#catch history for which shoal the fish were on
ch.spawner.shoal <- catch.hist.spawner %>%  
  pivot_wider(id_cols=c("LTFishIDAutonumber","sex"), names_from=yr, values_from=shoal,       
              values_fill="0", names_sort=TRUE)
head(ch.spawner.shoal) 

#go back and fill in shoals from 2015 prior in db
ch.spawner.shoal[, cols][is.na(ch.spawner.shoal[, cols] )]<- "0"
head(ch.spawner.shoal)

ch.spawner      $ch       <- do.call(paste, c(ch.spawner      [cols],sep=""))
ch.spawner.shoal$ch.shoal <- do.call(paste, c(ch.spawner.shoal[cols],sep=""))
headtail(ch.spawner)
headtail(ch.spawner.shoal)
length(cols) #this is the number of events


# first and last capture occasion for each fish
ch.spawner.shoal$firstCap <-   ifelse(rowSums(ch.spawner.shoal[,cols]!=0)==0, Inf, 
                                      max.col(ch.spawner.shoal[,cols]!="0", ties.method="first"))
ch.spawner.shoal$lastCap <-   ifelse(rowSums(ch.spawner.shoal[,cols]!=0)==0, -Inf, 
                                     max.col(ch.spawner.shoal[,cols]!="0", ties.method="last"))
head(as.data.frame(ch.spawner.shoal))


# now look at the pairs of transitions, i.e shoal in year i and shoal in year i+1
# between the first and last captures. You can't use the data before the first capture
# because the fish may not have been recruited yet.
# You can't use the fish after the last capture because the fish may be dead
#*** KP: currently not working properly-to fix
trans <- plyr::ldply(1:(nchar(ch.spawner.shoal$ch.shoal[1])-1), function(start){
  # extract 2 letter transitions starting at start
  res <-data.frame(id =ch.spawner.shoal$LTFishIDAutonumber,
                   sex=ch.spawner.shoal$sex,
                   firstCap=ch.spawner.shoal$firstCap,
                   lastCap =ch.spawner.shoal$lastCap,
                   yr = start,
                   trans=substr(ch.spawner.shoal$ch.shoal,start,start+1))
  #browser()
  # check the first and last capture times
  res <- res[ res$firstCap <= start & res$lastCap >= start+1,] 
  res
})

#KP - but doesn't the above take the first year (2008) capture status no matter if the fish has been 
# recruited or not?

trans$this.year.shoal <- substr(trans$trans,1,1)
trans$next.year.shoal <- substr(trans$trans,2,2)
head(trans)

# Look at the transition matrix
xtabs(~this.year.shoal+next.year.shoal+sex, data=trans, exclude=NULL, na.action=na.pass)

# females is so sparse as to be useless
# unknown sex is so sparse as to be useless
trans <- trans[ trans$sex == "m",]
xtabs(~this.year.shoal + next.year.shoal, data=trans, exclude=NULL, na.action=na.pass)
prop.table(xtabs(~this.year.shoal + next.year.shoal, data=trans, exclude=NULL, na.action=na.pass), margin=1)

# so, for males, it looks like some fish are sticky to their shoal
# for example, if are captured on shoal X, then most of the time if come back next year and 
#are captured you tend to come back to shoal X.
# Shoal Y and Z seem to be "lumped" together but sample sizes, esp for Z are very small.











####
##### Mark-recap Analysis - POPAN #### 
#*** Make sure that you have Mark downloaded onto your computer from here: 
# http://www.phidot.org/software/mark/.  

## The following lines are based on B. Anderson's scripts and 
##  updates in 2021 from C. Schwarz

#establish the process.data for analysis
####time intervals are fractions of a year between capture events, and includes the length of time elapsed since the last capture event -- must be updated to current time
####nocc is the number of capture occasions
####begin.time is set to 2008 to indicate the fall period of that year
#and create the default design data - ddl



ch.spawner <- catch.hist.spawner %>% 
  spread(key=yr, value = tot.catches,fill=0) 

cols <- names(ch.spawner)[5:ncol(ch.spawner)]
ch.spawner$ch <- do.call(paste, c(ch.spawner[cols],sep=""))
headtail(ch.spawner)
length(cols) #this is the number of events

#select only male spawners
ch.spawnerm <- ch.spawner %>% 
  filter(sex %in% "m")
headtail(ch.spawnerm)

## CJS. Note that RMark gets upset with tibbles
ch.spawnerm <- as.data.frame(ch.spawnerm)

####GOF global test ####
library(R2ucare)
ch.mat.m<- as.data.frame(ch.spawner[ ch.spawner$sex=="m", c(as.character(2008:2021))]) # UCARE does not like tibbles
overall_CJS(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))  #isn't freq mortality? pull from data?
## ** KP Note that there is a big ol long procedure for figuring out what is wrong if this
#     global test for GOF fails (see MR_Analysis_gof.R) but if it p is decently >0.05, no worries!
# seems like as of 2021 this test fails pretty hard... I didn't think it did last year so 
# need to investigate this.

moberly.proc = process.data(ch.spawnerm, model= "POPAN", begin.time=2008)
(moberly.ddl=make.design.data(moberly.proc))


#list the elements of the ddl dataframe
mode(moberly.ddl)
names(moberly.ddl)


#### POPAN Parameters ####
#to set up the possible parameters for later selection/inclusion within models
Phi.dot=list(formula=~1)  #constant survival
Phi.Time=list(formula=~Time)  #survival varying by time as a trend

p.dot=list(formula=~1) # capture probability constant
## CJS. Given the way that RMARK parameterizes all parameters, it will sometimes be sensible to use
##      a parameterization that models each parameter as individual parameters rather than as offset
##      from the first parameter. This is especially true when the first parameter (e.g. p(1) is weird, in this case
##      very little effort at first capture occasion.)
##      Results will be identical under this or the simple ~time parameterization but can be much more numerically stable
p.time   =list(formula=~-1+time)  #capture probability varies with event
p.time.cs=list(formula=~-1+time,fixed=list(time=2008,value=1))  #capture probability varies with event, initial p value set to 1 as it cannot be estimated... based on advise from Carl Schwarz
  #KP: in this case the "-1" doesn't consider the first year?

#pent.time=list(formula=~time)  #probability of entrance from the superpopulation varies with event... variable recruitment
## CJS.. The only sensible models for pent are t or perhaps . Does not make sense for a T model 
## CJS   This is because the pents must sum to zero 
pent.dot=list(formula=~1)    #probability of entrance from the superpopulation is constant...not considered as this requires that initial population size be equivalent to annual immigration thereafter 
#pent.Time=list(formula=~Time)    #probability of entrance from the superpopulation varies with time as a Trend

## CJS   I assume here you want a zero recruitment model. This formula doesn't work (look at estimates when the model is fit)
pent.zero   =list(formula=~1, fixed=~0)  #sets probablity of entrance to zero (zero recruitment)... added this in 2015 but still need to run it
## CJS   Note that pents are index for the END of the interval, i.e the pent for 2009 is the pent between 2008 and 2009 (sigh!).
pent.zero.cs=list(formula=~1, fixed=list(time=2009:2021,value=0)) 
pent.time   =list(formula=~time)   ## CJS

#run models with selected parameters of interest from above for S (=Phi), p and pent
#first six models represent the possible combinations of S and pent, with p varying by event
#difficulty pointed out with beta parameters by CS, seems to be an issue of confounding in first p, in fully time-varying models

#CS points out that in popan models with p(t), the first p is NEVER estimable... 
#so consider including his fix i all models even though it only seems to create problematic outputs in models 2 and 5   
##CJS don't fit a pent(T) models as it really doesn't make sense. 

model.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))

## CJS check total number of captures by year
colSums(ch.spawnerm[, as.character(2008:2021)])


## CJS It often helps to use the initial values from a simpler model when fitting a slightly more complex model
##     The message for model.2 is because some of the pent beta are highly negative with huge standard errors 
##     indicating that they are essentially zero. (logit(0)=-infinity on beta scale)
##     RMarks has trouble dealing with the very negative parameter.
#model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.Time))  ## CJS pent.Time doesn't make sense
model.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time.cs, pent=pent.time), initial=model.1)

## CJS I assume you want to test if no recritment is happening, i.e. a death only model. Your pent.zero didn't work
##     The model.3 gives NA for the quick results for pent, but if you look at the long real results everything is ok
##     This is because all parameters were fixed to 0. 
model.3.old=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero))
model.3.old$results$real
rm(model.3.old)


## CJS here is the proper model for no recruitment
model.3    =mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.zero.cs))
model.3$results$real

#model4: survival as linear time trend, encounter prob varies by yr, recruitment constant
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




### POPAN model averaging ####
#now average the results of the models using the AIC weighting
(moberly.JS.mod.avg=model.average(moberly.JS.results,vcv=TRUE))
names(moberly.JS.mod.avg)
popan.ests <- moberly.JS.mod.avg$estimates

## CJS. The model averaging below gives a warnings about a problem with the VCV for some 
##      models. THis is a result of pent parameters going to 0 in some models. 
##      Set the drop=FALSE term to avoid dropping model
(Phi.ests <- model.average(moberly.JS.results, "Phi")) #, vcv=TRUE

## CJS compare the two following model averaging. THe first one below drops one of the models, but you 
##     don't have to do that.
(pent.ests <- model.average(moberly.JS.results, "pent")) #, vcv=TRUE
(pent.ests <- model.average(moberly.JS.results, "pent", drop=FALSE)) #, vcv=TRUE

(p.ests <- model.average(moberly.JS.results, "p")) #, vcv=TRUE

#the support notes for Package 'RMark' provide background on the 'popan.derived' function on p 127
#it indicates that if a 'marklist' is provided (such as 'moberly.JS.results') the results are 
# model-averaged
class(moberly.JS.results)

(derived <- popan.derived(moberly.proc, moberly.JS.results, N=TRUE))
names(derived)
(Nbyocc <- derived$Nbyocc)
Nbyocc.plot <- Nbyocc %>% 
  mutate(occasion = 2008:2021)

library(ggplot2)
ggplot(Nbyocc.plot, aes(x=occasion, y=N)) +
  geom_point() +
  geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.1) +   
  #scale_y_continuous(breaks=seq(0.5,1.50, 0.25), limits=c(0.5,1.50)) +
  scale_x_continuous(breaks=seq(2008, 2021, 1)) +
  xlab(label = "Year") + ylab(label="N males (95% ci)") +   ## CJS indicate that bars are 95% ci
  theme_bw()





#### Mark-recap Analysis- Pradel ####

## CJS. The Pradel models are just a different parameterization of the JS model
##      THe key advantage is the ability to get lambda (population) growth directly
##      without having to do any computations.
##      See my article in the Gentle Introduction to MARK where I go through the various 
##      parameterization.

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

#### Pradel Parameters ####

## CJS only those that differ from the POPAN parameterization are shown below
lambda.dot =list(formula=~1)    #population growth rate is constant 
lambda.Time=list(formula=~Time)    #populations growth rate varies as a trend with time

#### Pradel models ####
#run models with selected parameters of interest from above for S (=Phi), p and lambda
#seems to be no confounding of parameters

## CJS All seem to run properly
pmodel.1=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.dot))
pmodel.2=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot,  p=p.time, Lambda=lambda.Time))
pmodel.3=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.dot))
pmodel.4=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.Time, p=p.time, Lambda=lambda.Time))


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

(Phi.ests <- model.average(moberly.pradel.results, "Phi")) #, vcv=TRUE
(p.ests   <- model.average(moberly.pradel.results, "p")) #, vcv=TRUE
(lambda.ests <- model.average(moberly.pradel.results, "Lambda")) #, vcv=TRUE

# for plotting
lambda.ests <- cbind(lambda.ests, moberly.pradel.mod.avg$estimates[moberly.pradel.mod.avg$estimates$par.index %in% lambda.ests$par.index,4:5]) %>% 
    mutate(plot.time = as.numeric(as.character(time))+0.5)

library(ggplot2)
ggplot(lambda.ests, aes(x=plot.time, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1) +   ## CJS I like narrower error bars
  scale_y_continuous(breaks=seq(0.5,1.50, 0.25), limits=c(0.5,1.50)) +
  scale_x_continuous(breaks=seq(2008, 2021, 1)) +
  xlab(label = "Year") + ylab(label="Lambda estimate (95% ci)") +   ## CJS indicate that bars are 95% ci
  geom_hline(yintercept=1, linetype="dashed" ) +
  theme_classic()









#### Barker Analysis - in DEVT ####

(catch.hist.byseasonyr08 <- catch.history %>%  
   filter(season %in% c("spring", "summer", "fall"), yr %in% c(2008:2016)) %>% 
   mutate(season.num = case_when(season %in% "spring" ~ "1spring",
                                 season %in% "summer" ~ "2summer",
                                 season %in% "fall" ~ "3fall")) %>% 
   mutate(season.yr = factor(paste0(yr,"-",season.num), ordered=T)) %>% 
   group_by(LTFishIDAutonumber, season.yr) %>% 
   dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count)) %>% 
   mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches)) %>% 
   arrange(LTFishIDAutonumber)) 


#capture histories from all seasons, all recap types:
(ch.allsampling <- spread(data=catch.hist.byseasonyr08, key=season.yr, value = tot.catches,
                          fill=0))

cols <- names(ch.allsampling)[4:ncol(ch.allsampling)]
ch.allsampling$ch <- do.call(paste, c(ch.allsampling[cols],sep=""))
headtail(ch.allsampling)


ch.allsampling %>% 
  filter(LTFishIDAutonumber %in% c(451,498,572)) %>% 
  as.data.frame()




#### RMark Clean-up ####

cleanup(ask=FALSE)





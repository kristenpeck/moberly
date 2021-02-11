# Look at goodness of fit statistics
# See https://sites.google.com/site/cmrsoftware/lecture-lab-schedule/6---model-fit-and-mmi/methods-for-assessing-fit

# There are two types of statistics
#   - the suite of chi-square tests from RELEASE and UCARE
#     These are useful for investigating particular feature, such 
#     if the probability of detection next year depends if detected this year.
#     This could fail if there is "skip" spawning.
#     These GOF are based on the capture-histories, and really only look
#     at the recaptures after initial tagging and release, i.e. the CJS portion of the
#     the model. There is usually insufficient information to examine new captures.
#     Refer to Giminez paper that I've uploaded
#     and http://www.phidot.org/software/mark/docs/book/pdf/chap5.pdf Section 5.4.1 for the test2 and test3 table
#     construction along with Section 5.5. for more details on UCARE
#     You can also get this from the release.gof() function in RMark but why bother 
#     See also https://jamesepaterson.github.io/jamespatersonblog/2020-05-20_gof_for_CJS

#   - omnibus goodness of fit test for a specific model
#     This looks at an overall lack of fit not specific to any cause

# This script is meant to analyze the mark-recapture data from Moberly Lake.
# Analysis originally written by B. Anderson
# Set-up and analysis modifications by K. Peck and R. Elsner

library(data.table)
library(FSA)
library(RMark)
library(tidyverse)
library(RODBC)
library(R2ucare)


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




###############################################################
###############################################################
###############################################################
# GOF testing using capture histories using tests for RELEASE and UCARE
# Refer to 
#    Gimenez, O, Lebreton, J‐D, Choquet, R, Pradel, R. 
#    R2ucare: An r package to perform goodness‐of‐fit tests for capture–recapture models. 
#    Methods Ecol Evol. 2018; 9: 1749– 1754. https://doi.org/10.1111/2041-210X.13014
#
# The UCARE package wants a matrix of capture histories rather than the character string

ch.mat.m<- as.data.frame(ch.spawner[ ch.spawner$sex=="m", c(as.character(2008:2020))]) # UCARE does not like tibbles
overall_CJS(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))

# look at overall components - refer to Gimenez paper for detail on what test does 


# Test3sr() Newly encountered individuals have the same chance to be later 
# reobserved as recaptured (previously encountered) individuals. usually a test for trap dependence
# happiness or shyness

test3sr(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))

# test2ct() Missed individuals have the same chance to be recaptured at the next occasion as 
# currently captured individuals; usually a test for transients

test2ct(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))


# test2cl. There is no difference in the timing of reencounters between the individuals encountered and not encountered at occasion i, 
# con-ditional on presence at both occasions i and i + 2

test2cl(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))

# test3sm. Among those individuals seen again, when they were seen does not differ among 
# previously and newly marked individuals; this is the null hypothesis of, for example, skip spawning

test3sm(ch.mat.m, freq=rep(1, nrow(ch.mat.m)))

##################
# female spawners - overall test fails because test3sm failes (see below)
ch.mat.f<- as.data.frame(ch.spawner[ ch.spawner$sex=="f", c(as.character(2008:2020))]) # UCARE does not like tibbles
#overall_CJS(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))

# look at overall components - refer to Gimenez paper for detail on what test does 


# Test3sr() Newly encountered individuals have the same chance to be later 
# reobserved as recaptured (previously encountered) individuals. usually a test for trap dependence
# happiness or shyness

test3sr(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))

# test2ct() Missed individuals have the same chance to be recaptured at the next occasion as 
# currently captured individuals; usually a test for transients

test2ct(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))


# test2cl. There is no difference in the timing of reencounters between the individuals encountered and not encountered at occasion i, 
# con-ditional on presence at both occasions i and i + 2

test2cl(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))

# test3sm. Among those individuals seen again, when they were seen does not differ among 
# previously and newly marked individuals; this is the null hypothesis of, for example, skip spawning
# fails for females because 
#test3sm(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))

# fails for females because data is just too sparse with only a hanful of individual seen 3+times.
apply(ch.mat.f, 1, sum)
marray(ch.mat.f, freq=rep(1, nrow(ch.mat.f)))


###############################################################
###############################################################
###############################################################
####
#### Goodness of fit testing using Bootstrap analysis
## Assume you have a model fit using POPAN.
## Pick the top rated model (or the fully time specified model if it runs)

# get the males
#select only male spawners
ch.spawnerm <- ch.spawner %>% 
  filter(sex %in% "m")
headtail(ch.spawnerm)


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


# we again create simulator to generate simulated data from this model and fit it using RMark
## Number of simulations to run. 200 is good enough for GOF
N.sim <- 200

## Intialize the random number generator
set.seed(23423)

# model 1 appears to be the "best" model
model.pop=mark(moberly.proc, moberly.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, pent=pent.dot))

# find the deviance statistics but these have no direct interpretation for popan models
model.pop$results$deviance
model.pop$results$deviance.df

# we need to bootstrap and see how bad deviance really is
# Extract the parameters estimates of the model

sample.times<-  model.pop$begin.time + c(0,cumsum(model.pop$time.intervals))
 
N       <- get.real(model.pop, "N")
p       <- get.real(model.pop, "p")
Phi     <- get.real(model.pop, "Phi")
pent    <- get.real(model.pop, "pent")

data.frame(times=sample.times, N=c(N, rep(NA,length(sample.times)-1)), p=as.vector(p), Phi=c(Phi,NA), pent=c(pent,NA))

# make any adjustments for unequal sampling times
## Compute the gaps between sampling occasions
gaps <- diff(sample.times)
gaps
length(gaps)

## and said that the next 3 phis are the average.
## Because the Phi are on a per.year basis, I need to adjust for the larger gaps
Phi  <- c(Phi)^gaps  # 
Phi
length(Phi)


## Function to simulate popan model

sim.popan <- function( sample.times, N, p, Phi, pent){
   gaps <- diff(sample.times)  # interval between sampling occasions
   # N is the total number of animals every seen in the population
   pop <- matrix(0, nrow=N, ncol=length(sample.times))  # initial members of the population x sample times
   # The number that enter before sample time is multinomial with pent parameters
   N.new <- rmultinom(1, size=N, prob=c(1-sum(pent),pent))
   start.row <- 1+cumsum(c(0,N.new))
   end.row   <- cumsum(c(N.new))
   # now generate survival catchability. This needs to be done sequentially
   pop[start.row[1]:end.row[1],1] <- 1 # initial population present
   for(time in 2:length(sample.times)){
      # survival
      pop[,time] <- pop[,time-1]*rbinom(nrow(pop),1, Phi[time-1] )
      pop[start.row[time]:end.row[time],time] <- 1  # when do the animals enter
    }
   
   # apply the capture probabilities at each sample time to generate the capture history matrix
   ch <- pop * matrix(rbinom( length(pop), size=1, rep(p, each=nrow(pop))), nrow=nrow(pop), ncol=ncol(pop))

   list(pop=pop, ch=ch)   
}

# Here is an example of the simulator in action
test.pop <- sim.popan( sample.times=sample.times, N=N, p=p, Phi=Phi, pent=pent)
nrow(test.pop$pop)
colSums(test.pop$pop)  # population size over time
colSums(test.pop$ch )  # captured over time

# compare P(capture) vs theoretical p
colSums(test.pop$ch) / colSums(test.pop$pop)
p

# compare proportion that enter
# new entry = no in pop at time i-1, in pop at time i
colSums(test.pop$pop*(0==cbind(0,test.pop$pop[,-ncol(test.pop$pop)])))/nrow(test.pop$pop)
c(1-sum(pent),pent)

# compare actual survival (must be alive a time i and time i+1) vs theoretical survival
colSums(test.pop$pop[,-ncol(test.pop$pop)]*test.pop$pop[,-1])/colSums(test.pop$pop[,-ncol(test.pop$pop)])
Phi


## This fits a Mark model using RMark
## caution, if a model does not coverge, the fit$results will be NULL. So  you need to check for this (see below)
fit.Mark <- function(ch, sample.times=sample.times){
    fit.proc = process.data(data.frame(ch=ch, freq=1), model= "POPAN", begin.time=2008, time.intervals=diff(sample.times))
    fit.ddl=make.design.data(fit.proc)
    # make any adjustments to the ddl here as needed,
    #browser()
    ## Specify the model that will be fit to the simulated data. 
    ## This should be the model we want the GOF for
    fit <- mark(fit.proc, ddl=fit.ddl, 
                model.parameters=list(Phi=Phi.dot, 
                                      p=p.time, 
                                      pent=pent.dot),  # this is a popan model that was chosen
                output=FALSE, delete=TRUE,silent=TRUE)
    #browser()
    fit
}



# test the fitting routine
select <- apply(test.pop$ch,1,any)  # at least one detection in the capture history
ch <- apply(test.pop$ch[select,],1, paste0, collapse="") # remove all zero capture histories
head(ch)
length(ch)

# do one fit
res<- fit.Mark(ch, sample.times=sample.times)

# check the estimates
get.real(res, parameter="p")
p

get.real(res, parameter="Phi")
Phi

get.real(res, parameter="pent")
pent


# now to put these together to do multiple simulations and store all of the results
## The value of N.sim is set at the top of the code and controls the number of simulations to do
## If a model doesn't converge, the sim$results will be null
sim <- plyr::llply(1:N.sim, function(sim,  sample.times, N, p, Phi, pent){
   if(sim %% 5 ==1)cat("Fitting simulation ", sim,"\n")
   pop <- sim.popan(sample.times=sample.times, N=N, p=p, Phi=Phi, pent=pent)
   select <- apply(pop$ch,1,any)  # at least one detection
   ch <- apply(pop$ch[select,],1, paste0, collapse="")
   res <- fit.Mark(ch, sample.times=sample.times)
   res$sim <- sim
   res
}, sample.times=sample.times, N=N, p=p, Phi=Phi, pent=pent)

## Here is how you can check out a specific simulation.
# check out certain simulations
sim.number <- 18

names(sim[[sim.number]])
sim[[sim.number]]$singular
sim[[sim.number]]$data
sim[[sim.number]]$output
sim[[sim.number]]$results
sim[[sim.number]]$results$beta
sim[[sim.number]]$results$real
get.real(sim[[sim.number]], parameter="N")
sim[[sim.number]]$sim

# Now we extract the deviance from each of the popan model and see how bad
# our actual data is

sim.real.est <- plyr::ldply(sim, function(x){
    if(x$sim %% 10 ==1)cat("Extracting from simulation ", x$sim,"\n")
    if( is.null(x$results)){
       # simulation didn't converge
       cat("Simulation ", x$sim, " failed to converge. No ouput \n")
       return(NULL)
    }
    df0 <- data.frame(estimate=c(x$results$deviance, NULL), Year=0, parameter="deviance")
    res <- get.real(x, parameter="p")
    df1 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="p")
    res <- get.real(x, parameter="Phi")
    df2 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="Phi")
    res <- get.real(x, parameter="pent")
    df3 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="pent")
    rbind(df0, df1, df2, df3)
})
head(sim.real.est)

# what fraction of simulated deviance exceed our deviance - this is the gof statistic
gof.p.value <- mean(model.pop$results$deviance< sim.real.est$estimate[sim.real.est$parameter=="deviance"])

ggplot(data=sim.real.est[sim.real.est$parameter=="deviance",], aes(x=estimate))+
  ggtitle("Deviance from simulated POPAN models")+
  geom_histogram( alpha=0.2)+
  geom_vline(xintercept=model.pop$results$deviance)+
  annotate("text", label=paste0("P(sim deviances exceeding obs deviance): ",round(gof.p.value,2)),
              x=-Inf, y=-Inf, hjust=0, vjust=-0.5)
######################################################################

# You can do a similar GOF for the pradel model, but if the POPAN model fits, then the PRADEL model will also fit.


cleanup(ask=FALSE)

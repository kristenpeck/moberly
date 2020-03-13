
# This script is for general analysis to do with CPUE, length at age, general fish summaries
# It does not include the Mark-Recap population analysis- this is in MR_Analysis

#Authir: Rick Elsner/ Kristen Peck

### Note: to run the Mob_Db_connect you must be using the 
# 32 bit version of R (can change this in Tools-> Global Options). Or the 64-bit version, depending on your driver

#setwds
# outputs2018 <- "//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/R Scripts & Outputs/2018-2019/Analysis/"
# Db.connect <- "//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/lakes/Moberly Lake/Data & Analysis/R Scripts & Outputs/2018-2019/Analysis/"

library(FSA)
library(plyr); library(dplyr)


# library(data.table)
# library(FSA)
# library(RMark)
# library(RODBC)
# library(car)

options(digits=4) #limits printed numbers to 4 sig. digits


########################################
#### Run Moberly LT Database script ####
########################################

# run DB_connect script to load and clean the data from the database.
source("DB_connect.R")
ls()


#### Summarize basic data ####

str(effort.catch)

year = 2019

unique(effort.catch$survey.type)
survey = "Spawner Sampling/Tagging"


# how many LT were caught in given year during a given survey?

temp <- effort.catch %>% 
  filter(species %in% "LT") %>% 
  filter(yr %in% year) %>% 
  filter(survey.type %in% survey)

summarise(temp, 
          tot.nets = length(unique(EffortAutoNumber)),
          tot.uniqueLT = length(unique(LTFishIDAutonumber)),
          tot.males = length(which(sex %in% "m")))

temp$LTFishIDAutonumber[which(duplicated(temp$LTFishIDAutonumber))]


#############################################
#### Summarize - FL - FALL SPAWNER Males ####
#############################################
# FL for Spawner surveys by year for Males only 2008 and later after more effort 
# put into mark recapture netting
str(effort.catch)
levels(effort.catch$survey.type)
unique(effort.catch$yr)

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   sex=="m") %>%
           mutate(fyr=as.factor(yr),
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

headtail(summary <- tmp %>%  
           group_by(yr) %>% 
           summarize(meanFL=round(mean(FL,na.rm=TRUE),2),
                     seFL=round(se(FL,na.rm=TRUE),2),
                     nFL=sum(count),
                     meanWT=round(mean(WT,na.rm=TRUE),2),
                     seWT=round(se(WT,na.rm=TRUE),2),
                     nWT=sum(count),
                     meanK=round(mean(condition,na.rm=TRUE),2),
                     secondition=round(se(condition,na.rm=TRUE),2),
                     nK=sum(count)) %>% 
           mutate(fyr=as.factor(yr),
                  lciFL=round(meanFL-qt(1-(0.05/2),nFL-1)*seFL,2),
                  uciFL=round(meanFL+qt(1-(0.05/2),nFL-1)*seFL,2)) %>% 
           as.data.frame())
summary2 <- summary %>% filterD(yr>=2008)

##########################################################
# ANOVA male LT fork length in fall mark-recap survey ####
##########################################################

library(car)
require(multcomp)
summary(mod.FL <- aov(FL ~ fyr, data=tmp, subset=yr>=2008))
plot(mod.FL$residuals~mod.FL$fitted.values) # check assumptions normality/homogeneity
summary(tuks <- glht(mod.FL, linfct = mcp(fyr = "Tukey"))) # Tukey test multiple comparisons
tuk.cld <- cld(tuks)   # letter-based display of significant differences
sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
colnames(sig) <- "sigletter" # change row name for plotting
summary2 <- cbind(sig,summary2) # bind data sets
str(summary2)

#####################################
#### Plot - mean male spawner FL ####
#####################################
library(ggplot2)

setwd(outputs2018)
png("0Male_mean_fork_length_2008-2018.png",width = 800, height = 550)

(ggplot(summary, aes(x=as.factor(yr), y=meanFL)) +
    geom_point(data=summary, size=4, shape=1) +
    geom_point(data=summary2, size=4) +
    geom_errorbar(data=summary,width=0.2, aes(ymin=meanFL-seFL,ymax=meanFL+seFL)) +
    geom_text(data=summary2, label=summary2$sigletter, aes(y=uciFL+25)) +
    geom_text(label=summary$nFL,aes(y=400), hjust=-0.1) + 
    ylab(label="Mean Male Fork Length (mm)") + xlab(label="Year") +
    scale_y_continuous(limits=c(400,800),breaks=seq(400,800,50)) +
    theme_classic(base_size = 16) +
    geom_text(label=paste("n="),aes(y=400),hjust=1))

dev.off()




###############################################
#### Summarize - FL - FALL SPAWNER females ####
###############################################
# FL for Spawner surveys by year for females only 2008 and later after more effort 
# put into mark recapture netting

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling"|survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   sex=="f") %>%
           mutate(fyr=as.factor(yr),
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

headtail(summary <- tmp %>% group_by(yr) %>% 
           summarize(meanFL=round(mean(FL,na.rm=TRUE),2),
                     seFL=round(se(FL,na.rm=TRUE),2),
                     nFL=sum(count),
                     meanWT=round(mean(WT,na.rm=TRUE),2),
                     seWT=round(se(WT,na.rm=TRUE),2),
                     nWT=sum(count),
                     meanK=round(mean(condition,na.rm=TRUE),2),
                     secondition=round(se(condition,na.rm=TRUE),2),
                     nK=sum(count)) %>% 
           mutate(fyr=as.factor(yr),
                  lciFL=round(meanFL-qt(1-(0.05/2),nFL-1)*seFL,2),
                  uciFL=round(meanFL+qt(1-(0.05/2),nFL-1)*seFL,2)) %>% 
           as.data.frame())
summary2 <- summary %>% filterD(yr>=2008)

############################################################
# ANOVA female LT fork length in fall mark-recap survey ####
############################################################

summary(mod.FL <- aov(FL ~ fyr, data=tmp, subset=yr>=2008))
plot(mod.FL$residuals~mod.FL$fitted.values) # check assumptions normality/homogeneity
summary(tuks <- glht(mod.FL, linfct = mcp(fyr = "Tukey"))) # Tukey test multiple comparisons
tuk.cld <- cld(tuks)   # letter-based display of significant differences
sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
colnames(sig) <- "sigletter" # change row name for plotting
summary2 <- cbind(sig,summary2) # bind data sets
str(summary2)

#######################################
#### Plot - mean female spawner FL ####
#######################################

setwd(outputs2018)
png("0Female_mean_fork_length_2008-2018.png",width = 800, height = 550)

(ggplot(summary, aes(x=as.factor(yr), y=meanFL)) +
  geom_point(data=summary, size=4, shape=1) +
  geom_point(data=summary2, size=4) +
  geom_errorbar(data=summary,width=0.2, aes(ymin=meanFL-seFL,ymax=meanFL+seFL)) +
  geom_text(data=summary2,label=summary2$sigletter,aes(y=uciFL+25)) + 
  geom_text(label=summary$nFL,aes(y=450), hjust=-0.1) + 
  ylab(label="Mean Female Fork Length (mm)") + xlab(label="Year") +
  scale_y_continuous(limits=c(450,980),breaks=seq(450,980,100)) +
  theme_classic(base_size = 16) +
  geom_text(label=paste("n="),aes(y=450),hjust=1))

dev.off()

################
# SLIN CPUE #### 
################
headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="SLIN - Spring Littoral Index Netting") %>%
           mutate(fyr=as.factor(yr),
                  species=as.character(species),
                  soaktime=as.numeric(soaktime))) #use filter to analyze different years, sizes, lakes, etc
levels(tmp$maturity)

# count number of each species by unique effort
headtail(tmp <- tmp %>% 
           group_by(EffortAutoNumber,species) %>% 
           summarize(count=sum(count)) %>% 
           as.data.frame()) 

tmp$species <- ifelse(tmp$species=="NFC","",tmp$species) # covert NFC to ""
tmp$count <- ifelse(tmp$count=="NA",0,tmp$count) # convert NA counts to 0's

# IMPORTANT Add zero catch for nets with no catch of certain species
headtail(tmp <- tmp %>% 
           addZeroCatch("EffortAutoNumber","species",zerovar="count") %>% 
           arrange(EffortAutoNumber,species))

# calculate mean net fishing effort
headtail(soak <- effort.catch %>% 
           group_by(EffortAutoNumber,yr) %>% 
           summarize(soaktime=mean(soaktime)) %>% 
           mutate(soaktime=as.numeric(soaktime)))

# join dataframes to calculate CPUE
headtail(tmp <- left_join(tmp,soak,by="EffortAutoNumber") %>% 
           mutate(cpue=count/soaktime,
                  fyr=as.factor(yr)))
str(tmp)
# summarize CPUE
(summary <- tmp %>% 
    filterD(species=="LT") %>% 
    group_by(yr,species) %>% 
    summarize(n=validn(EffortAutoNumber), 
              meancpue=round(mean(cpue),2),
              sd=round(sd(cpue),2),
              se=round(se(cpue),2),
              min=min(cpue),
              max=max(cpue)) %>%
    mutate(se=round(sd/sqrt(n),2),
           lci=round(meancpue-qt(1-(0.05/2),n-1)*se,2),
           uci=round(meancpue+qt(1-(0.05/2),n-1)*se,2)) %>%
    as.data.frame())

str(summary)

######################
# ANOVA SLIN CPUE ####
######################

summary(mod.SLIN <- aov(cpue ~ fyr, data=tmp))
plot(mod.SLIN$residuals~mod.SLIN$fitted.values) # check assumptions normality/homogeneity
#tuks <- glht(mod.SLIN, linfct = mcp(fyr = "Tukey")) # Tukey test multiple comparisons
#tuk.cld <- cld(tuks)   # letter-based display of significant differences
#sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
#colnames(sig) <- "sigletter" # change row name for plotting
#summary <- cbind(sig,summary) # bind data sets
#str(summary)

# NOTE FOR ABOVE, ASSUMPTIONS OF ANOVA VIOLATED SEE RESIDUAL PLOT
# SIGNIFICANT DIFFERENCES NOT SHOWN IN PLOT BELOW

###############################
#### Plot - mean SLIN CPUE ####
###############################

setwd(outputs2018)
png("0mean_SLIN_CPUE_2008-2018.png",width = 800, height = 550)

(ggplot(summary, aes(x=as.factor(yr), y=meancpue)) +
    geom_point(size=4) +
    geom_errorbar(data=summary,width=0.1, aes(ymin=meancpue-se,ymax=meancpue+se)) +
#   geom_text(label=summary$sigletter,aes(y=uci+0.02)) + 
    geom_text(label=summary$n,aes(y=0), hjust=-0.1) + 
    ylab(label="Mean Catch-per-unit-effort (# fish/net-hr)") + xlab(label="Year") +
    scale_y_continuous(limits=c(0,0.5),breaks=seq(0,0.5,0.1)) +
  theme_classic(base_size = 16) +
  geom_text(label=paste("n="),aes(y=0),hjust=1))

dev.off()


#######################
# Mean Male LT Age ####
#######################

# Male Spawner Age
str(effort.catch)

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling"|survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   sex=="m") %>%
           mutate(fyr=as.factor(yr),
                  age=age2,
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

headtail(summary <- tmp %>% group_by(yr) %>% 
           summarize(meanAge=round(mean(age,na.rm=TRUE),2),
                     seAge=round(se(age,na.rm=TRUE),2),
                     nAge=sum(count)) %>% 
           mutate(fyr=as.factor(yr),
                  lciAge=round(meanAge-qt(1-(0.05/2),nAge-1)*seAge,2),
                  uciAge=round(meanAge+qt(1-(0.05/2),nAge-1)*seAge,2)) %>% 
           as.data.frame())
summary2 <- summary %>% filterD(yr>=2008)

###############################################
# ANOVA male LT Age fall mark-recap survey ####
###############################################

summary(mod.Age <- aov(age ~ fyr, data=tmp, subset=yr>=2008))
plot(mod.Age$residuals~mod.Age$fitted.values) # check assumptions normality/homogeneity
tuks <- glht(mod.Age, linfct = mcp(fyr = "Tukey")) # Tukey test multiple comparisons
tuk.cld <- cld(tuks)   # letter-based display of significant differences
sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
colnames(sig) <- "sigletter" # change row name for plotting
summary2 <- cbind(sig,summary2) # bind data sets
str(summary2)

######################################
#### Plot - mean male spawner Age ####
######################################

setwd(outputs2018)
png("0Male_mean_age_2008-2018.png",width = 800, height = 550)

(ggplot(summary, aes(x=as.factor(yr), y=meanAge)) +
    geom_point(data=summary, size=4, shape=1) +
    geom_point(data=summary2, size=4) +
    geom_errorbar(data=summary,width=0.2, aes(ymin=meanAge-seAge,ymax=meanAge+seAge)) +
    geom_text(data=summary2, label=summary2$sigletter,aes(y=uciAge+1)) + 
    geom_text(label=summary$nAge,aes(y=0), hjust=-0.1) + 
    ylab(label="Mean Male Age (years)") + xlab(label="Year") +
    scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2)) +
    theme_classic(base_size = 16) +
    geom_text(label=paste("n="),aes(y=0),hjust=1))

dev.off()

#########################
# Mean Female LT Age ####
#########################

# Female Spawner Age
str(effort.catch)

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling"|survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   sex=="f") %>%
           mutate(fyr=as.factor(yr),
                  age=age2,
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

headtail(summary <- tmp %>% group_by(yr) %>% 
           summarize(meanAge=round(mean(age,na.rm=TRUE),2),
                     seAge=round(se(age,na.rm=TRUE),2),
                     nAge=sum(count)) %>% 
           mutate(fyr=as.factor(yr),
                  lciAge=round(meanAge-qt(1-(0.05/2),nAge-1)*seAge,2),
                  uciAge=round(meanAge+qt(1-(0.05/2),nAge-1)*seAge,2)) %>% 
           as.data.frame())
summary2 <- summary %>% filterD(yr>=2008)

#################################################
# ANOVA female LT Age fall mark-recap survey ####
#################################################

summary(mod.Age <- aov(age ~ fyr, data=tmp, subset=yr>=2008))
plot(mod.Age$residuals~mod.Age$fitted.values) # check assumptions normality/homogeneity
tuks <- glht(mod.Age, linfct = mcp(fyr = "Tukey")) # Tukey test multiple comparisons
tuk.cld <- cld(tuks)   # letter-based display of significant differences
sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
colnames(sig) <- "sigletter" # change row name for plotting
summary2 <- cbind(sig,summary2) # bind data sets
str(summary2)

########################################
#### Plot - mean female spawner Age ####
########################################

setwd(outputs2018)
png("0Female_mean_age_2008-2018.png",width = 800, height = 550)

(ggplot(summary, aes(x=as.factor(yr), y=meanAge)) +
    geom_point(data=summary, size=4, shape=1) +
    geom_point(data=summary2, size=4) +
    geom_errorbar(data=summary,width=0.2, aes(ymin=meanAge-seAge,ymax=meanAge+seAge)) +
    geom_text(data=summary2, label=summary2$sigletter,aes(y=uciAge+1)) + 
    geom_text(label=summary$nAge,aes(y=0), hjust=-0.1) + 
    ylab(label="Mean Female Age (years)") + xlab(label="Year") +
    scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2)) +
    theme_classic(base_size = 16) +
    geom_text(label=paste("n="),aes(y=0),hjust=1))

dev.off()

####################################
# Length-at-Age von Bertalanffy ####
####################################
library(nlstools)
library(FSA)
str(effort.catch)

# Male von Bertalanffy ####
headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling"|survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   sex=="m", !is.na(age2),!is.na(FL)) %>%
           mutate(fyr=as.factor(yr),
                  age=age2,
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

(sv <- vbStarts(FL~age,data=tmp,plot=FALSE,methLinf=c("Walford"))) # starting values
vb <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0))) # von bert function
fitvb <- nls(FL~vb(age,Linf,K,t0),data=tmp,start=sv) # von bert fit
predvals <- data.frame(age=seq(0,max(tmp$age),length.out=99)) # range of values to predict vb fit
predvals$FL <- predict(fitvb,newdata=predvals) # predicted values based on vb coefficients
coef(fitvb) # vb coefficients
(vbconf <- confint(fitvb)) # vb coefficient confidence intervals
#summary(bootvb <- nlsBoot(fitvb,niter=1000))
#(vbbootests <- bootvb$estiboot)
#(vbbootestci <- confint(bootvb,plot=FALSE))
summary(fitvb,correlation = TRUE) # summary of model
p.low <- vb(predvals$age,Linf=confint(fitvb)[1,1],K=confint(fitvb)[2,1],t0=confint(fitvb)[3,1]) # lower 95 % ci
p.high <- vb(predvals$age,Linf=confint(fitvb)[1,2],K=confint(fitvb)[2,2],t0=confint(fitvb)[3,2]) # upper 95% co

predvals <- cbind(predvals,p.low,p.high) # bind
str(predvals)

# both sexes, consider breaking up by sex, there should be differences
setwd(outputs2018)
png("0Male_vonBert_ALLYears.png",width = 600, height = 550)

(ggplot(tmp,aes(x=age,y=FL)) + 
    geom_jitter(aes(colour=fyr), width=0.2, size=1.5) +
    geom_line(data=predvals, size=1) +
    geom_line(data=predvals, aes(x=age,y=p.low), size=1, linetype="dashed") +
    geom_line(data=predvals, aes(x=age,y=p.high), size=1, linetype="dashed") +
    scale_x_continuous(breaks=seq(0,30,5), limits=c(0,30)) +
    scale_y_continuous(breaks=seq(200,1000,100), limits=c(200,1000)) +
    xlab(label="Age") + ylab(label="Fork Length (mm)") +
    theme_classic() +
    guides(colour = guide_legend(title = "Survey Year"))) 

dev.off()

# Males vs Females vb ####

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="Spawner Sampling"|survey.type=="Spawner Sampling/Tagging",
                   species=="LT",
                   !sex=="un"&!sex=="nc", !is.na(age2),!is.na(FL)) %>%
           mutate(fyr=as.factor(yr),
                  age=age2,
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc
tmpm <- filterD(tmp, sex=="m")

(sv <- vbStarts(FL~age,data=tmpm,plot=FALSE,methLinf=c("Walford"))) # starting values
vb <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0))) # von bert function
fitvb <- nls(FL~vb(age,Linf,K,t0),data=tmpm,start=sv) # von bert fit
predvals <- data.frame(age=seq(0,max(tmpm$age),length.out=99)) # range of values to predict vb fit
predvals$FL <- predict(fitvb,newdata=predvals) # predicted values based on vb coefficients
coef(fitvb) # vb coefficients
(vbconf <- confint(fitvb)) # vb coefficient confidence intervals
#summary(bootvb <- nlsBoot(fitvb,niter=1000))
#(vbbootests <- bootvb$estiboot)
#(vbbootestci <- confint(bootvb,plot=FALSE))
summary(fitvb,correlation = TRUE) # summary of model
p.low <- vb(predvals$age,Linf=confint(fitvb)[1,1],K=confint(fitvb)[2,1],t0=confint(fitvb)[3,1]) # lower 95 % ci
p.high <- vb(predvals$age,Linf=confint(fitvb)[1,2],K=confint(fitvb)[2,2],t0=confint(fitvb)[3,2]) # upper 95% co

predvals <- cbind(predvals,p.low,p.high) # bind
predvals$sex <- "m"

tmpf <- filterD(tmp, sex=="f")

(sv <- vbStarts(FL~age,data=tmpf,plot=FALSE,methLinf=c("Walford"))) # starting values
vb <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0))) # von bert function
fitvb <- nls(FL~vb(age,Linf,K,t0),data=tmpf,start=sv) # von bert fit
predvals1 <- data.frame(age=seq(0,max(tmpf$age),length.out=99)) # range of values to predict vb fit
predvals1$FL <- predict(fitvb,newdata=predvals1) # predicted values based on vb coefficients
coef(fitvb) # vb coefficients
(vbconf <- confint(fitvb)) # vb coefficient confidence intervals
#summary(bootvb <- nlsBoot(fitvb,niter=1000))
#(vbbootests <- bootvb$estiboot)
#(vbbootestci <- confint(bootvb,plot=FALSE))
summary(fitvb,correlation = TRUE) # summary of model
p.low <- vb(predvals1$age,Linf=confint(fitvb)[1,1],K=confint(fitvb)[2,1],t0=confint(fitvb)[3,1]) # lower 95 % ci
p.high <- vb(predvals1$age,Linf=confint(fitvb)[1,2],K=confint(fitvb)[2,2],t0=confint(fitvb)[3,2]) # upper 95% co

predvals1 <- cbind(predvals1,p.low,p.high) # bind
predvals1$sex <- "f"

tmp1 <- rbind(predvals, predvals1)

# both sexes

setwd(outputs2018)
png("0Male-Female_vonBert_ALLYears.png",width = 600, height = 550)

(ggplot(tmp,aes(x=age,y=FL,colour=sex)) + 
    geom_jitter(width=0.2,size=1.5) +
    geom_line(data=tmp1, aes(x=age,y=FL, colour=sex), size=1) +
    geom_line(data=tmp1, aes(x=age,y=p.low, colour=sex), size=1, linetype="dashed") +
    geom_line(data=tmp1, aes(x=age,y=p.high, colour=sex), size=1, linetype="dashed") +
    scale_x_continuous(breaks=seq(0,30,5), limits=c(0,30)) +
    scale_y_continuous(breaks=seq(0,1000,100), limits=c(0,1000)) +
    xlab(label="Age") + ylab(label="Fork Length (mm)") +
    theme_classic() +
    guides(colour = guide_legend(title = "Sex"))) 

dev.off()



###############################################################
###############################################################
# MODIFIED UNTIL HERE #########################################


################################################################
############## Hatchery Fish Condition ##########################
################################################################

# this part needs work and more probing in DB to evaluate if cohorts are even recorded properly

#size cut-off of juvenile K. Pecks value from 2016 analysis
#juv.size <- 484 # smallest mature fish detected in spawner surveys

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   species=="LT",
                   !cohort=="NA",
                   !survey.type=="Hatchery Survival at Release") %>%
           mutate(fyr=as.factor(yr),
                  age=age2,
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc
str(tmp)
summary(tmp$FL)


#### Hatchery Condition ####

setwd(outputs2018)
png("0Hatchery_Fish_Condition.png", width = 600, height = 550)

(ggplot(tmp, aes(x=yr, y=condition, colour=as.factor(cohort))) +
    geom_jitter(width=0.2) +
    xlab(label="Year") + ylab(label="Fulton's Condition Factor") +
    scale_y_continuous(breaks=seq(0.25,1.5,0.1), limits=c(0.25,1.5)) +
    theme_classic() +
    guides(colour = guide_legend(title = "Cohort")))

dev.off()

#### Hatchery Fork Lengths #### 
#indicates some growth of recaps, particularly for cohort 2

setwd(outputs2018)
png("0Hatchery__Fish_Fork_Length.png", width = 600, height = 400)

(ggplot(tmp, aes(x=yr, y=FL, colour=as.factor(cohort))) +
    geom_jitter(width=0.2) +
    xlab(label="Year") + ylab(label="Fork Length (mm)") +
    scale_y_continuous(breaks=seq(100,500,50), limits=c(100,500)) +
    theme_classic() +
    guides(colour = guide_legend(title = "Cohort")))

dev.off()



######################################
######## Mark-Recapture ##############
######################################

# see MR_Analysis script
####

######################################
# SLIN Mean Male FL ####
######################################

levels(effort.catch$survey.type)

headtail(tmp  <- effort.catch %>% 
           filterD(EffortAutoNumber,
                   survey.type=="SLIN - Spring Littoral Index Netting",
                   species=="LT",
                   sex=="m") %>%
           mutate(fyr=as.factor(yr),
                  species=as.character(species))) #use filter to analyze different years, sizes, lakes, etc

headtail(summary <- tmp %>%  
           group_by(yr) %>% 
           summarize(meanFL=round(mean(FL,na.rm=TRUE),2),
                     seFL=round(se(FL,na.rm=TRUE),2),
                     nFL=sum(count),
                     meanWT=round(mean(WT,na.rm=TRUE),2),
                     seWT=round(se(WT,na.rm=TRUE),2),
                     nWT=sum(count),
                     meanK=round(mean(condition,na.rm=TRUE),2),
                     secondition=round(se(condition,na.rm=TRUE),2),
                     nK=sum(count)) %>% 
           mutate(fyr=as.factor(yr),
                  lciFL=round(meanFL-qt(1-(0.05/2),nFL-1)*seFL,2),
                  uciFL=round(meanFL+qt(1-(0.05/2),nFL-1)*seFL,2)) %>% 
           as.data.frame())

##########################################################
# ANOVA SLIN male LT fork length survey ####
##########################################################

library(car)
require(multcomp)
summary(mod.FL <- aov(FL ~ fyr, data=tmp))
plot(mod.FL$residuals~mod.FL$fitted.values) # check assumptions normality/homogeneity
tuks <- glht(mod.FL, linfct = mcp(fyr = "Tukey")) # Tukey test multiple comparisons
tuk.cld <- cld(tuks)   # letter-based display of significant differences
sig <- data.frame(tuk.cld$mcletters$Letters) # convert to data.frame
colnames(sig) <- "sigletter" # change row name for plotting
summary <- cbind(sig,summary) # bind data sets
str(summary)

# NOTE NO SIG DIFFS ABOVE
#####################################
#### Plot - SLIN mean male  FL ####
#####################################
library(ggplot2)

(ggplot(summary, aes(x=as.factor(yr), y=meanFL)) +
    geom_point(size=4) +
    geom_errorbar(data=summary,width=0.2, aes(ymin=lciFL,ymax=uciFL)) +
    #geom_text(data=summary, label=summary$sigletter, aes(y=uciFL+25)) +
    geom_text(label=summary$nFL,aes(y=200), hjust=-0.1) + 
    ylab(label="Mean Male Fork Length (mm)") + xlab(label="Year") +
    scale_y_continuous(limits=c(200,1000),breaks=seq(200,1000,50)) +
    theme_classic(base_size = 16) +
    geom_text(label=paste("n="),aes(y=200),hjust=1))

#############################
# Individual fish growth #### # started but not completed
#############################
levels(effort.catch$survey.type)
headtail(tmp  <- effort.catch %>% 
           filterD(species=="LT",
                   sex=="m",
                   !is.na(FL)) %>% 
           mutate(fyr=as.factor(yr),
                  LTID=as.factor(LTFishID_Autonumber))%>%
           arrange(LTID,fyr))

headtail(tmp1 <- tmp %>%
           group_by(LTID) %>%
           summarize(recaps=validn(fyr)) %>% as.data.frame())

headtail(summary <- tmp %>%  
           group_by(LTID,yr) %>%
           summarize(meanFL=mean(FL,na.rm=FALSE)) %>% mutate(recaps=NA) %>% 
           arrange(LTID,yr) %>%
           as.data.frame())
 
for (i in 1:nrow(summary)) {
  summary$recaps[i] <- tmp1[which(tmp1$LTID==summary$LTID[i]),"recaps"]
}

library(ggplot2)
str(tmp)
names(summary)

sum <- summary %>% 
  filterD(recaps>=0)

(ggplot(sum, aes(x=yr, y=meanFL, group=LTID, colour=LTID)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks=seq(2005,2018,1),limits=c(2005,2018)) +
    xlab(label="Year") + ylab(label="Mean Fork Length (mm)") +
    theme_classic())



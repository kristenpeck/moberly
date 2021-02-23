# Look at fidelity/mixing at the spawning grounds
# We need to modify the capture history to record which spawning area is used

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
suppressWarnings(source("Db_connect.R"))
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

# look at shoal codes
xtabs(~yr+shoal, data=catch.history, exclude=NULL, na.action=na.pass)
# is a single fish captured on more than one shoal in a year? Only a handful of cases...

n.shoals <- catch.history %>% 
  filter(survey.type %in% "Spawner Sampling/Tagging", 
         gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                          "Seine Net", "Angling")) %>% 
  dplyr::group_by(LTFishIDAutonumber, yr) %>% 
  filter(!is.na(LTFishIDAutonumber)) %>% 
  dplyr::summarize(n.shoals = length(unique(shoal)),shoals = paste(unique(shoal), collapse=""))
  

n.shoals[ n.shoals$n.shoals > 1,] # KP: the fish where multiple shoals were visited in one yr.



# find the shoal where it was captured
#df and capture histories on spawning shoals only (excluding holding pen):
catch.hist.spawner <- catch.history %>%  
  filter(survey.type %in% "Spawner Sampling/Tagging", 
         gear.type %in% c("SLIN - Spring Littoral Index Netting Gillnet",
                          "Seine Net", "Angling")) %>% 
  dplyr::group_by(LTFishIDAutonumber, yr) %>% 
  dplyr::summarise(sex=unique(sex), freq=last(freq), tot.catches = sum(count), 
                             shoal=unique(shoal)[1]) %>%  ## CJS added dplyr::
  mutate(tot.catches = ifelse(tot.catches >1, 1, tot.catches),
         shoal = shoal) %>% 
  arrange(LTFishIDAutonumber, yr) 
catch.hist.spawner


# notice that in some years, the shoal is missing even if a fish is captured.
# for example look at fish 108 in 201. # KP fixed most of these 22-Feb-2021


## CJS YOu want to remove the 2005, 2006, 2007 data and start with 2008 as the
##     number of captures in the first 3 years is very small
plyr::ddply(catch.hist.spawner, "yr", plyr::summarize, n.fish=length(yr))
catch.hist.spawner <- catch.hist.spawner[ catch.hist.spawner$yr >= 2008,]

ch.spawner       <- catch.hist.spawner %>% 
      pivot_wider(id_cols=c("LTFishIDAutonumber","sex"), names_from=yr, values_from=tot.catches, values_fill=0, names_sort=TRUE)
head(ch.spawner)
ch.spawner.shoal <- catch.hist.spawner %>%  
      pivot_wider(id_cols=c("LTFishIDAutonumber","sex"), names_from=yr, values_from=shoal,       values_fill="0", names_sort=TRUE)
head(ch.spawner.shoal)



cols <- names(ch.spawner)[-(1:2)]
cols

ch.spawner.shoal[, cols][is.na(ch.spawner.shoal[, cols] )]<- "0"
head(ch.spawner.shoal)

ch.spawner      $ch       <- do.call(paste, c(ch.spawner      [cols],sep=""))
ch.spawner.shoal$ch.shoal <- do.call(paste, c(ch.spawner.shoal[cols],sep=""))
headtail(ch.spawner)
headtail(ch.spawner.shoal)
length(cols) #this is the number of events

# first and last capture occassion for each fish
ch.spawner.shoal$firstCap <-   ifelse(rowSums(ch.spawner.shoal[,cols]!=0)==0, Inf, 
                                   max.col(ch.spawner.shoal[,cols]!="0", ties.method="first"))
ch.spawner.shoal$lastCap <-   ifelse(rowSums(ch.spawner.shoal[,cols]!=0)==0, -Inf, 
                                   max.col(ch.spawner.shoal[,cols]!="0", ties.method="last"))
head(as.data.frame(ch.spawner.shoal))


#see if any fish in the dataset have no captures since these will not work in MR analysis
which(ch.spawner$ch == paste(rep(0,length(4:ncol(ch.spawner))-1),collapse =""))

# now look at the pairs of transitions, i.e shoal in year i and shoal in year i+1
# between the first and last captures. You can't use the data before the first capture
# because the fish may not have been recruited yet.
# You can't use the fish after the last capture because the fish may be dead
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
# for example, if are captured on shoal X, then most of the time if come back next year and are captured
# you tend to come back to shoal X.
# Shoal Y and Z seem to be "lumped" together but sample sizes, esp for Z are very small.




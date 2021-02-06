## Power analysis and effect of design modifications on capture-recapture estimates
##
## This illustrates how to examine the impact of changing sampling methods (e.g. measure every 2 years)
## on precision of the estimates from a capture-recapture experiment.
## You can also use to estimate the power to detect an effect (e.g. a changepoint) by fitting a change point model
## (See my notes on the web about change point models)

## This is done using simulation
## I've only used 50 simulations to estimate precision and power. In "real life', I suggest at least 200 (and preferably 500
## simulations. The N.sim variable at the start controls this)

## We will illustrate an example of detecing a change point in 2020 where lambda is changing compared to previous trend
library(tidyverse)
library(plyr)
library(RMark)

## Number of simulations to run
N.sim <- 50

## Intialize the random number generator
set.seed(23423)

## I got the parameter estimates from te MR_Analysis_CS.R file which saved the model averaged
## values to the workspace. However, you can just put in your parameter values.

# load the model averaged estimates from the pradel model
load( file="moberly.pradel.ma.Phi.RData")
load( file="moberly.pradel.ma.p.RData")
load( file="moberly.pradel.ma.Lambda.RData")

# values of the parameters
Phi.ests
p.ests
Lambda.ests

## You need to be a bit careful about the lambda and phi terms.
## Conceptually lambda = phi + f 
## where "f" is a fecundity parameter. Consequently, lambda cannot go below phi
## However, the Pradel models don't enforce this condition. 
## Are there any problem areas? Look for negative values for the difference
Lambda.ests$estimate - Phi.ests$estimate

## Now we will set the parameter values for our simulation.
## If you want to skip a year (e.g. sample every 2 years, just set the "year" value to the appropriate value
## However, when you skip years, you need to be careful of the phi and lambda values. '
## These are normally on a "per year" basis, so if the gap between sampling occasions is > 1, year
## you need to adjust the lambda and phi values as shown below

## Here we add 3 more sampling times in 2, 4, and 6 years after the last sampling occasion
sample.times <- c(p.ests$Time, max(p.ests$Time)+c(2,4,6))  # 3 more sampling occasions with 2 year gaps
sample.times

## Compute the gaps between sampling occasions
gaps <- diff(sample.times)
gaps
length(gaps)

## Specify the Phi vales. Here I used the estimates from the capture recaptur model
## and said that the next 3 phis are the average.
## Because the Phi are on a per.year basis, I need to adjust for the larger gaps
Phi  <- c(Phi.ests$estimate, rep(mean(Phi.ests$estimate),3))^gaps  # add 3 more sample occasions with avg survival but raised to gap size
Phi
length(Phi)

## Specify the capture probability in the future. Here I used the average
p <- c(p.ests$estimate, rep(mean(p.ests$estimate),3))              # add 3 more sample occasions with mean catchability
p
length(p)

## Specify a lambda value.
## we will have a lambda that is steady state after 2020
Lambda.trend <- lm(Lambda.ests$estimate ~ Lambda.ests$Time)
Lambda <- c(Lambda.ests$estimate,   rep(1,3) )^gaps
Lambda

## Put all the parameters together.
## Notice these have been adjusted for unequal sampling intervals
# parameters for the simulations. Notice that Phi and Lambda have been adjusted for any increase in time between samples at the end
parms <- cbind(Year=2008+sample.times, sample.times, p, Phi=c(Phi,NA),Lambda=c(Lambda,NA))
parms


## Function to simulate population given an initial population size and the various parameters
##
# We will simulate a population of initial size of around 200. This is not that crucial.
sim.pop <- function( N.init=200, sample.times, p, Phi, Lambda){
   gaps <- diff(sample.times)  # interval between sampling occasions
   pop <- matrix(NA, nrow=N.init, ncol=length(sample.times))  # initial members of the population x sample times

   pop[,1] <- 1  # initial population alive
   # now generate survival and population growth. This needs to be done sequentially
   for(time in 2:length(sample.times)){
      # survival
      pop[,time] <- pop[,time-1]*rbinom(nrow(pop),1, Phi[time-1] )
      # generate new animals using poisson distribution
      new.animals <- rpois(1, sum(pop[,time-1])*max(0,(Lambda[time-1]-Phi[time-1]))) # lambda >= phi must be enforced
      new.pop <- cbind(matrix(0, nrow=new.animals, ncol=time-1),
                       matrix(1, nrow=new.animals, ncol=1),
                       matrix(NA,nrow=new.animals, ncol=length(sample.times)-time))
      pop <- rbind(pop, new.pop)
   }
   
   # apply the capture probabilities at each sample time to generate the capture history matrix
   ch <- pop * matrix(rbinom( length(pop), size=1, rep(p, each=nrow(pop))), nrow=nrow(pop), ncol=ncol(pop))

   list(pop=pop, ch=ch)   
}

# Here is an example of the simulator in action
test.pop <- sim.pop( N.init=200, sample.times=sample.times, p=p, Phi=Phi, Lambda=Lambda)
nrow(test.pop$pop)
colSums(test.pop$pop)  # population size over time
colSums(test.pop$ch )  # captured over time

# compare P(capture) vs theoretical p
colSums(test.pop$ch) / colSums(test.pop$pop)
p

# compare actual Lambda vs theoretical Lambda
# there may be problems the end when lambda and phi are not consistent with each other
colSums(test.pop$pop[,-1])/ colSums(test.pop$pop[,-ncol(test.pop$pop)])
Lambda

# compare actual survival (must be alive a time i and time i+1) vs theoretical survival
colSums(test.pop$pop[,-ncol(test.pop$pop)]*test.pop$pop[,-1])/colSums(test.pop$pop[,-ncol(test.pop$pop)])
Phi


## This function generates a simulated population and then fits a Mark model using RMark
## caution, if a model does not coverge, the fit$results will be NULL. So  you need to check for this (see below)
fit.Mark <- function(ch, sample.times=sample.times){
    fit.proc = process.data(data.frame(ch=ch, freq=1), model= "Pradlambda", begin.time=2008, time.intervals=diff(sample.times))
    fit.ddl=make.design.data(fit.proc)
    # make any adjustments to the ddl here as needed,. e.g. for a change point model 
    fit.ddl$Lambda$Time.cp <- pmax(0,as.numeric(as.character(fit.ddl$Lambda$time))-2020)
    #browser()
    ## Specify the model that will be fit to the simulated data. 
    ## Here used a constant Phi, varying p, and a change point model for lambda.
    ## The change point is set by constructing the Time.cp variable in the ddl above. 
    ## The change point starts in 2020
    fit <- mark(fit.proc, ddl=fit.ddl, 
                model.parameters=list(Phi=   list(formula=~1),  
                                      p=     list(formula=~-1+time), 
                                      Lambda=list(formula=~Time+Time.cp)),  # this is a change point with a v-sahpe
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

# check the fit
res$design.data$Lambda

# check the estimates
get.real(res, parameter="p")
p

get.real(res, parameter="Phi")
Phi

get.real(res, parameter="Lambda")
Lambda


# now to put these together to do multiple simulations and store all of the results
## The value of N.sim is set at the top of the code and controls the number of simulations to do
## If a model doesn't converge, the sim$results will be null
sim <- plyr::llply(1:N.sim, function(sim, N.init, sample.times, p, Phi, Lambda){
   if(sim %% 5 ==1)cat("Fitting simulation ", sim,"\n")
   pop <- sim.pop(N.init=N.init, sample.times=sample.times, p=p, Phi=Phi, Lambda=Lambda)
   select <- apply(pop$ch,1,any)  # at least one detection
   ch <- apply(pop$ch[select,],1, paste0, collapse="")
   res <- fit.Mark(ch, sample.times=sample.times)
   res$sim <- sim
   res
}, N.init=200, sample.times=sample.times, p=p, Phi=Phi, Lambda=Lambda)

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
get.real(sim[[sim.number]], parameter="Lambda")
sim[[sim.number]]$sim

## Extract the real estimates from all of the simulations
## If a model didn't converge, ignore it.
# Extract the results and plot them
sim.real.est <- plyr::ldply(sim, function(x){
    if(x$sim %% 10 ==1)cat("Extracting from simulation ", x$sim,"\n")
    if( is.null(x$results)){
       # simulation didn't converge
       cat("Simulation ", x$sim, " failed to converge. No ouput \n")
       return(NULL)
    }
    res <- get.real(x, parameter="p")
    df1 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="p")
    res <- get.real(x, parameter="Phi")
    df2 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="Phi")
    res <- get.real(x, parameter="Lambda")
    df3 = data.frame(estimate=c(res,NULL), Year=as.numeric(colnames(res)), parameter="Lambda")
    rbind(df1, df2, df3)
})
head(sim.real.est)


# Compute mean of the real estimates, the standard error, and 95% ci using empirical estimates
sim.real.est.summary <- plyr::ddply(sim.real.est, c("parameter","Year"), function(x){
     mean = mean(x$estimate, na.rm=TRUE)
     se   = sd  (x$estimate, na.rm=TRUE)
     lcl  = quantile(x$estimate, prob=.025,  na.rm=TRUE)
     ucl  = quantile(x$estimate, prob=.975,  na.rm=TRUE)
     data.frame(mean, se, lcl, ucl)
})
sim.real.est.summary

# plot the expected precision of the results. 
# are these adequate for your purposes - not a statistical questions
ggplot(data=sim.real.est.summary, aes(x=Year, y=mean))+
   ggtitle("Estimates from simulation")+
   geom_point()+
   geom_line()+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
   facet_wrap(~parameter, ncol=2, scale="free_y")+
   scale_x_continuous(breaks=seq(1990,2100,2))


# Suppose you were interested in the power to detect the change point
# You need to extract the beta estimates to see if we detect an effect
sim.beta.est <- plyr::ldply(sim, function(x){
    if(x$sim %% 10 ==1)cat("Extracting from simulation ", x$sim,"\n")
    if( is.null(x$results)){
       # simulation didn't converge
       cat("Simulation ", x$sim, " failed to converge. No ouput \n")
       return(NULL)
    }
    res <- x$results$beta
    res$beta.parameter <- row.names(res)
    res$sim <- x$sim
    res
})
head(sim.beta.est)


# power to detect a change for example,
unique(sim.beta.est$beta.parameter)

# In a change point model, the Lambda:Time.cp parameter is a test for a change point
# see what fraction of time the 95% ci excludes the value of 0 for the power at alpha=0.05.
beta.change.point <- sim.beta.est[ sim.beta.est$beta.parameter == "Lambda:Time.cp",]
head(beta.change.point)
beta.change.point$detect <- beta.change.point$lcl>0

# compute fraction of simulations where ci excluded 0 to estimate the power
mean(beta.change.point$detect)


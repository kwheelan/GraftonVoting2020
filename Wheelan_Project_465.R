
#====================================================================================================
#STAT 465
#Katrina Wheelan
#Fall 2020

# Presidential voting patterns in Grafton County, New Hampshire


# (1) Package Requirements:
# tidyr, dplyr, sf, ggplot2, nimble, coda, basicMCMCplots
#====================================================================================================

library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(nimble)
library(coda)
library(basicMCMCplots)

#====================================================================================================
# (2) Importing data

# Data sources: NH Department of State, Census Bureau's 2018 American Community Survey

# I pre-processed all data and published it to a public repo here: 
# https://github.com/kwheelan/GraftonVoting2020

# The csv can be downloaded directly from the url, but the shape files (for visuals) 
# must be downloaded to your hard drive first.

# A note on the data:
#   There are 19 census tracts in Grafton County. I've combined the two tracts in
#   Hanover and the two in Lebanon because there was only town-level voting data. 
#   This leaves us with 17 "tracts."

# A note on the variables:
#   age = median age in the tract
#   ed = proportion of the adult population that has at least some college
#   race/propwhite = proportion of the population that identifies as white
#   income = median household income
#   Biden.votes.2020 = total votes for Joseph Biden and Kamala Harris in the 
#                    2020 general presidential election
#   total.2020 = total votes cast for any candidate in the tract in the
#                    2020 general presidential election

#====================================================================================================

grafton <- read.csv("https://raw.githubusercontent.com/kwheelan/GraftonVoting2020/main/processed_grafton_data.csv")

# if you want visuals, change this to your filepath after download from repo
geo_data <- st_read("C:/Users/katri/Desktop/Stat 465/grafton_geo_data.shp")

#====================================================================================================
# (3) Preliminary Model

# This model uses fixed effects for education, race, age, and log(income),
# as well as a random, tract-level effect for a logistic regression model 
# of the probability of voting for Biden in each tract. It then uses a 
# binomial likelihood for the number of Biden votes in each tract.

#====================================================================================================

code <- nimbleCode({
  
  # Normal priors for each of the fixed effects coefficients
  b0 ~ dnorm(0, sd = 100)
  b_ed ~ dnorm(0, sd=100)
  b_race ~ dnorm(0, sd=100)
  b_age ~ dnorm(0, sd=100)
  b_income ~ dnorm(0, sd=100)
  
  # Uniform prior for sd of random effect
  sigma_tract ~ dunif(0, 10000)
  
  for (i in 1:nTracts){
    # Random effect for each tract
    epsilon_tract[i] ~ dnorm(0, sd = sigma_tract)
  }
  
  for (i in 1:nTracts){
    # logit equation for the proportion of Biden votes
    logit(p[i]) <- b0 + 
      b_ed * ed[i] +
      b_race * propwhite[i] + 
      b_age * age[i] + 
      b_income * income[i] + 
      epsilon_tract[i]
    
    # binomial likelihood for the number of Biden votes in each tract
    y[i] ~ dbinom(prob = p[i], size=nVoters[i])
  }
  
})

# See details on variables in comment block 2
constants <- list(nTracts= 17, # 17 tracts/towns in the county
                  ed = grafton$ed, 
                  propwhite = grafton$prop.white,
                  age = grafton$med.age,
                  income = log(grafton$median.household.income), 
                  nVoters = grafton$total.2020)

data <- list(y = grafton$biden.votes.2020)

# initializing all coefficients to 0 and sigma_tract to 1
inits <- list(b0=0, 
              sigma_tract=1, 
              b_ed = 0,
              b_race = 0,
              b_age = 0,
              b_income = 0,
              epsilon_tract=replicate(17, 0))

# compiling
Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

#====================================================================================================

# (4) Evaluation of Premilinary Model

# The mixing looks okay, but the effective sample size is quite low (less
# than 2 for b_income). A 95% BCI for b_income includes
# zero, and the coefficient for b_age is extremely small, so we will remove
# these covariates in the next model to see if any metrics improve.

#====================================================================================================

# getting 10,000 samples with 10,000 more as burn-in
samples <- runMCMC(Cmcmc, 20000, nburnin=10000)

# some helpful diagnostic tools to assess mixing, etc
samplesPlot(samples)
effectiveSize(samples)
samplesSummary(samples)


#====================================================================================================
# (5) Reduced Model

# This model is similar to the first, but removes fixed effects for 
# log(income) and for median age.

#====================================================================================================

code <- nimbleCode({
  
  # Normal priors for each of the fixed effects coefficients
  b0 ~ dnorm(0, sd = 100)
  b_ed ~ dnorm(0, sd=100)
  b_race ~ dnorm(0, sd=100)

  # Uniform prior for sd of random effect
  sigma_tract ~ dunif(0, 10000)
  
  for (i in 1:nTracts){
    # Random effect for each tract
    epsilon_tract[i] ~ dnorm(0, sd = sigma_tract)
  }
  
  for (i in 1:nTracts){
    # logit equation for the proportion of Biden votes
    logit(p[i]) <- b0 + 
      b_ed * ed[i] +
      b_race * propwhite[i] + 
      epsilon_tract[i]
    
    # binomial likelihood for the number of Biden votes in each tract
    y[i] ~ dbinom(prob = p[i], size=nVoters[i])
  }
  
})

# See details on variables in comment block 2
constants <- list(nTracts= 17, # 17 tracts/towns in the county
                  ed = grafton$ed, 
                  propwhite = grafton$prop.white,
                  nVoters = grafton$total.2020)

data <- list(y = grafton$biden.votes.2020)

# initializing all coefficients to 0 and sigma_tract to 1
inits <- list(b0=0, 
              sigma_tract=1, 
              b_ed = 0,
              b_race = 0,
              epsilon_tract=replicate(17, 0))

# compiling
Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel, enableWAIC = TRUE)
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

#====================================================================================================

# (6) Evaluation of Reduced Model

# The mixing doesn't look great, and the effective sample size is quite low.
# b_race and b_0 are also quite closely correlated.
# We will center the covariates in the next model to see if any metrics 
# improve.

#====================================================================================================

# getting 40,000 samples with 10,000 more as burn-in
samples <- runMCMC(Cmcmc, 50000, nburnin=10000)

# some helpful diagnostic tools to assess mixing, etc
samplesPlot(samples)
effectiveSize(samples)
samplesSummary(samples)
cor(samples)


#====================================================================================================
# (7) Final Model

# This model is similar to the last, but centers the covariates.

#====================================================================================================

# Centering covariates
centered = grafton
centered$prop.white = grafton$prop.white - mean(grafton$prop.white)
centered$ed = grafton$ed - mean(grafton$ed)

constants <- list(nTracts= 17, # 17 tracts/towns in the county
                  ed = centered$ed, 
                  propwhite = centered$prop.white,
                  nVoters = grafton$total.2020)

# compiling
Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
conf$addMonitors("epsilon_tract")
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

#====================================================================================================

# (8) Evaluation of Final Model

# The effective samples size is significantly improved (and we also run 
# more simulations). 

# Although the coefficient on race (proportion of the population that is
# white) includes zero in its 95% BCI, it is still meaningful, so I kept 
# it in the model.

# I use 3 chains to get a better sense of convergence. The Gelman diagnostic
# gives us values relatively close to 1. Overall, the chain plots and the 
# other metrics suggest that convergence is reasonable.

#====================================================================================================

# getting 90,000 samples with 10,000 more as burn-in
samples <- runMCMC(Cmcmc, 100000, nburnin=10000)

# some helpful diagnostic tools to assess mixing, etc
samplesPlot(samples[,c(1:3, 21)])
effectiveSize(samples[,c(1:3, 21)])
samplesSummary(samples[,c(1:3, 21)])


# Using multiple chains to assess convergence better
initsFunction <- function() {
  list(b0 = rnorm(0, 100),
       b_ed = rnorm(0, 100),
       b_race = rnorm(0,100),
       sigma_tract = runif(0, 10000))
}
samplesList <- runMCMC(Cmcmc, 20000, nchains = 3, nburnin = 2000, inits = initsFunction, samplesAsCodaMCMC = T)

chainsPlot(samplesList[,c(1:3,21)])
gelman.diag(samplesList[,c(1:3,21)])

#====================================================================================================
# (9) Conclusions and Plots (requires shape file)

# Overall, we can say that tracts with more diversity (a smaller 
# white proportion of the population) are more likely to vote for Biden,
# although we should note that 0 is in the 95% BCI. An increase in 
# educational attainment is associated with a statistically significant
# increase in the proportion of Biden votes.

# We can look at the relative weight of the effects graphically using the
# plot code below.

#====================================================================================================

# storing the posterior means
b0.mean = samplesSummary(samples)[1,1]
b.ed.mean = samplesSummary(samples)[2,1]
b.race.mean = samplesSummary(samples)[3,1]

# weighting each tract's value by the posterior mean coefficient
geo_data$weighted.ed = centered$ed[c(1:16,10,17,14)] * b.ed.mean
geo_data$weighted.race = centered$prop.white[c(1:16,10,17,14)] * b.race.mean
geo_data$intercept = b0.mean
geo_data$biden.share.2020 = grafton$biden.share.2020
geo_data$rand.effects = samplesSummary(samples)[c(4:19,13,20,17),1]

# Creating a plotting function for our dataset
plot.effect <- function (var, title, limits=c(-1.25,1.25)) {
  ggplot(geo_data, aes(fill = var), color = "white") +
    geom_sf() +
    coord_sf() +  
    scale_fill_gradient2(high="blue", 
                         low="red", 
                         mid="white", 
                         midpoint=mean(limits),
                         limits= limits) +
    ggtitle(title) + 
    labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
         caption = "Data Sources: NH Dept of State; 2018 ACS")
}

#Plot relevant data and magnitude of effects
plot.effect(geo_data$biden.share.2020, "Biden Vote Share 2020", limits=c(0,1))
plot.effect(geo_data$intercept, "Intercept")
plot.effect(geo_data$weighted.ed, "Effect of Educational Attainment")
plot.effect(geo_data$weighted.race, "Effect of Diversity")
plot.effect(geo_data$rand.effects, "Random Effects")

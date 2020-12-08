library(tidyr)
library(dplyr)
library(tidycensus)
library(sf)
library(ggplot2)

grafton <- get_acs(year=2018, geography = "tract", state="NH", county="Grafton", geometry = TRUE,
                   variables = c(median.household.income = "B19013_001"))

other.vars <- get_acs(year=2018, geography = "tract", state="NH", county="Grafton", geometry = TRUE,
                   variables = c(med.age = "B01002H_001",
                                 total = "B03002_001",
                                 white = "B03002_003"))

grafton$med.age = other.vars$estimate[other.vars$variable == "med.age"]
grafton$prop.white = other.vars$estimate[other.vars$variable == "white"] / 
                     other.vars$estimate[other.vars$variable == "total"]
  
ggplot(grafton, aes(fill = prop.white, color = prop.white)) +
  geom_sf() +
  coord_sf() +  scale_fill_viridis(direction=-1, option="magma") +
  scale_color_viridis(direction=-1, option="magma") + ggtitle("Median HH Income")

grafton$town = c("Bethlehem, Sugar Hill, Franconia", 
                 "Canaan",
                 "Ellsworth, Rumney, Campton",
                 "Holderness and Ashland",
                 "Orange, Alexandria, Grafton",
                 "Littleton",
                 "Haverhill",
                 "Bridgewater and Bristol",
                 "Enfield",
                 "Hanover",
                 "Lincoln, Livermore, Waterville Valley, Thorton",
                 "Benton, Woodstock, Warren",
                 "Piermont, Orford, Wentworth",
                 "Lebanon",
                 "Lyme, Dorchester, Groton, Hebron",
                 "Monroe, Lyman, Lisbon, Bath, Landaff, Easton",
                 "Hanover",
                 "Plymouth",
                 "Lebanon")
                 
town.votes = read.csv("C:/Users/katri/Downloads/grafton.pres.2020.csv")
votes.2016 = read.csv("C:/Users/katri/Downloads/grafton2016.csv")

colnames(town.votes) <- c("town.name", "Trump","Biden","Other","total","X2")
colnames(votes.2016) <- c("town.name", "Trump","Biden","Other","total","X2")

for (i in 2:6){
  votes.2016[,i] = as.numeric(gsub(",", "", votes.2016[,i]))
}
votes.2016$total[is.na(votes.2016$total)]= 0
votes.2016 = votes.2016[!is.na(town.votes$Biden),]
votes.2016$Other = votes.2016$Other + votes.2016$total + votes.2016$X2
votes.2016$total = votes.2016$Biden + votes.2016$Trump + votes.2016$Other
votes.2016$town.name[grepl("Lebanon", votes.2016$town.name, fixed=T)] = "Lebanon"

town.votes$Biden = as.numeric(gsub(",", "", town.votes$Biden))
town.votes$Trump = as.numeric(gsub(",", "", town.votes$Trump))
town.votes$Other = as.numeric(gsub(",", "", town.votes$Other))
town.votes$total = town.votes$Biden + town.votes$Trump + town.votes$Other
town.votes = town.votes[!is.na(town.votes$Biden),]
town.votes$town.name[grepl("Lebanon", town.votes$town.name, fixed=T)] = "Lebanon"

grafton$biden.votes = 0
grafton$trump.votes = 0
grafton$other.votes = 0

for (i in 1:19){
  for (j in 1:dim(town.votes)[1]) {
    if (grepl(town.votes$town.name[j], grafton$town[i], fixed=T)){
      grafton$biden.votes.2020[i] = grafton$biden.votes[i] + town.votes$Biden[j]
      grafton$trump.votes.2020[i] = grafton$trump.votes[i] + town.votes$Trump[j]
      grafton$other.votes.2020[i] = grafton$other.votes[i] + town.votes$Other[j]
      grafton$biden.votes.2016[i] = grafton$biden.votes[i] + votes.2016$Biden[j]
      grafton$trump.votes.2016[i] = grafton$trump.votes[i] + votes.2016$Trump[j]
      grafton$other.votes.2016[i] = grafton$other.votes[i] + votes.2016$Other[j]
    }
  }
}

grafton$total.2020 = grafton$biden.votes.2020 + grafton$trump.votes.2020 + grafton$other.votes.2020
grafton$biden.share.2020 = grafton$biden.votes.2020 / grafton$total.2020
grafton$trump.share.2020 = grafton$trump.votes.2020 / grafton$total.2020
grafton$other.share.2020 = grafton$other.votes.2020 / grafton$total.2020
grafton$biden.margin.2020 = (grafton$biden.share.2020 - grafton$trump.share.2020) * 100

grafton$total.2016 = grafton$biden.votes.2016 + grafton$trump.votes.2016 + grafton$other.votes.2016
grafton$biden.share.2016 = grafton$biden.votes.2016 / grafton$total.2016
grafton$trump.share.2016 = grafton$trump.votes.2016 / grafton$total.2016
grafton$other.share.2016 = grafton$other.votes.2016 / grafton$total.2016
grafton$biden.margin.2016 = (grafton$biden.share.2016 - grafton$trump.share.2016) * 100

grafton$biden.margin.diff = grafton$biden.margin.2020 - grafton$biden.margin.2016

ggplot(grafton, aes(fill = biden.margin.2016), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", low="red", mid="white", midpoint=0) +
  ggtitle("Biden Margin 2016 in Grafton County, NH")

ggplot(grafton, aes(fill = biden.margin.diff), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", low="red", mid="white", midpoint=0) +
  ggtitle("Change in Biden Margin in Grafton County, NH")


ggplot(grafton, aes(fill = trump.share), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0.5) +
  ggtitle("Share of Trump Votes in Grafton County, NH")



ggplot(grafton, aes(fill = other.share), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(low="white", high="orange") +
  ggtitle("Share of Other Votes in Grafton County, NH")


ggplot(grafton, aes(fill = biden.margin), color = "gray") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", low="red", mid="white", midpoint=0, space = "Lab") +
  ggtitle("Biden Margins in Grafton County, NH")

#==================================================
#Models
#==================================================

grafton.data = grafton[,7:18]
grafton.data$med.hh.inc = grafton$estimate
grafton.data$high.inc = grafton.data$med.hh.inc > 60000
grafton.data = grafton.data[-c(17,19),]

ed <- get_acs(year=2018, geography = "tract", state="NH", county="Grafton", geometry = TRUE,
              variables = c(total = "B06009_001",
                            some.college = "B06009_004",
                            college = "B06009_005",
                            grad.degree = "B06009_006"))

grafton$ed = (ed$estimate[ed$variable == "some.college"] + 
  ed$estimate[ed$variable == "college"] + 
  ed$estimate[ed$variable == "grad.degree"]) / 
  ed$estimate[ed$variable == "total"] 

grafton.data$ed = grafton$ed[-c(17,19)]

library(nimble)
library(coda)
library(basicMCMCplots)


centered = grafton.data
centered$biden.votes = grafton.data$biden.votes - mean(grafton.data$biden.votes)
centered$med.hh.inc = grafton.data$med.hh.inc - mean(grafton.data$med.hh.inc)
centered$prop.white = grafton.data$prop.white - mean(grafton.data$prop.white)
centered$ed = grafton.data$ed - mean(grafton.data$ed)

grafton.data$total.2020 = grafton.data$biden.votes.2020 + grafton.data$trump.votes.2020 + grafton.data$other.votes.2020

code <- nimbleCode({
  
  b0 ~ dnorm(0, sd = 100)
  b_ed ~ dnorm(0, sd=100)
  b_race ~ dnorm(0, sd=100)
  
  sigma_tract ~ dunif(0, 10000)
  
  for (i in 1:nTracts){
    epsilon_tract[i] ~ dnorm(0, sd = sigma_tract)
  }
  
  for (i in 1:nTracts){
    logit(p[i]) <- b0 + 
      b_ed * ed[i] +
      b_race * propwhite[i] + 
      epsilon_tract[i]
    y[i] ~ dbinom(prob = p[i], size=nVoters[i])
  }
  
})

constants <- list(nTracts= 17, 
                  income = grafton.data$med.hh.inc, 
                  ed = centered$ed,
                  propwhite = centered$prop.white,
                  nVoters = grafton.data$total.2020)

data <- list(y = grafton.data$biden.votes.2020)

inits <- list(b0=0, 
              sigma_tract=1, 
              b_ed = 0,
              b_race = 0,
              epsilon_tract=replicate(17, 0))

Rmodel <- nimbleModel(code, constants, data, inits)

conf <- configureMCMC(Rmodel)
conf$addMonitors("epsilon_tract")
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)


samples <- runMCMC(Cmcmc, 500000, nburnin=2000)
samplesPlot(samples[, c(1,2,3,21)])
effectiveSize(samples[,c(1,2,3,21)])
samplesSummary(samples[,c(1,2,3,21)])
cor(samples[,c(1,2,3,21)])

samplesSummary(samples)[4:20,1] -> random.tract.effects
grafton$rand.effects = 0
grafton$rand.effects[1:16] = random.tract.effects[1:16]
grafton$rand.effects[17] = random.tract.effects[10] #Hanover 2
grafton$rand.effects[19] = random.tract.effects[14] #Leb 2
grafton$rand.effects[18] = random.tract.effects[17] #Plymouth

b0.mean = samplesSummary(samples)[1,1]
b.ed.mean = samplesSummary(samples)[2,1]
b.race.mean = samplesSummary(samples)[3,1]

grafton$weighted.ed = centered$ed[c(1:16,10,17,14)] * b.ed.mean
grafton$weighted.race = centered$prop.white[c(1:16,10,17,14)] * b.race.mean
grafton$intercept = b0.mean

ggplot(grafton, aes(fill = intercept), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=0,
                       limits= c(-1,1)) +
  ggtitle("Intercept") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")



ggplot(grafton, aes(fill = rand.effects), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=0,
                       limits= c(-1,1)) +
  ggtitle("Random Tract-Level Effects") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")


ggplot(grafton, aes(fill = weighted.ed), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=0,
                       limits= c(-1,1)) +
  ggtitle("Educational Attainment Effect") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")


ggplot(grafton, aes(fill = weighted.race), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=0,
                       limits= c(-1,1)) +
  ggtitle("Racial Diversity Effect") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")


ggplot(grafton, aes(fill = logit(biden.share.2020)), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=logit(0.5))+
  ggtitle("Share of Votes for Biden") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")


ggplot(grafton, aes(fill = biden.margin), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_gradient2(high="blue", 
                       low="red", 
                       mid="white", 
                       midpoint=0) +
  ggtitle("Biden Vote Margin") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")




ggplot(grafton, aes(fill = biden.margin.2020), color = "white") +
  geom_sf() +
  coord_sf() +  
  scale_fill_viridis() +
  ggtitle("Educational Attainment") + 
  labs(subtitle ="Grafton County, New Hampshire, 2020 General Election",
       caption = "Data Sources: NH Dept of State; 2018 ACS")



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
grafton.data$total.2020 = grafton.data$biden.votes.2020 + grafton.data$trump.votes.2020 + grafton.data$other.votes.2020

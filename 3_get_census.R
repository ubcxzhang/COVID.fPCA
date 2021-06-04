##############################################################################################
# Created on Mon Apr 13 10:27:59 2020
# Download US census data and perform the association analysis with COVID-19 infection pattern.
# @author: Xiaojian Shao, National Research Council Canada.
##############################################################################################

library(tidycensus)
library(tidyverse)

# read land area data from U.S. Census QuickFacts 2010
# special from https://web.archive.org/web/20150807220054/ http://quickfacts.census.gov/qfd/download_data.html
# we know it is "," separated.
nVariables <-
  max(count.fields("data/DataSet.txt", sep = ","))
geo <- read.table(
  "data/DataSet.txt",
  sep = ",",
  header = T,
  colClasses = c("character", rep("numeric", nVariables - 1))
)
landarea <- geo$LND110210
names(landarea) <- geo$fips

# load US census variable
# US census data: https://api.census.gov/data.html
# check codes from https://www2.census.gov/about/training-workshops/2019/2019-06-26-api-webinar.R
acs.2019 <-
  load_variables(
    year = 2019,
    dataset = "acs5",
    cache = TRUE
  )
# View(acs.2019) # use view and filter to search for different variables
nCounty <- 3220

## population
TotalPop <-
  get_acs(
    geography = "county",
    variables = "B01003_001E",
    survey = "acs5"
  )
TotalPopEst <- TotalPop$estimate

## population density
# by consider the landsize per county, calculate the population density
# match the county Ids between two tables
rownames(TotalPop) <- TotalPop$GEOID

landareaCountyIntersect <-
  intersect(TotalPop$GEOID, geo$fips) # intersection between two fips lists
landareaCountyAcsIndex <-
  match(landareaCountyIntersect, TotalPop$GEOID) # get the acs table index
landareaCountyIndex <-
  match(landareaCountyIntersect, geo$fips) # match the land area data index

PopDensity <- rep(NA, length(TotalPopEst))
PopDensity[landareaCountyAcsIndex] <-
  TotalPopEst[landareaCountyAcsIndex] / landarea[landareaCountyIndex]
PopDensity[-landareaCountyAcsIndex] <- NA

## sex by age
SexByAge <-
  get_acs(
    geography = "county",
    variables = "B01001_001E",
    survey = "acs5"
  )
SexByAgeMale <-
  get_acs(
    geography = "county",
    variables = "B01001_002E",
    survey = "acs5"
  )
# SexByAgeFemale <- get_acs(geography = "county", variables = "B01001_026E", survey = "acs5")
MaleProp <- SexByAgeMale$estimate / SexByAge$estimate

## race
# set up all the variables you want to fetch.
RacevariableSets <-
  c(
    "B02001_001E",
    "B02001_002E",
    "B02001_003E",
    "B02001_004E",
    "B02001_005E",
    "B02001_006E",
    "B02001_007E"
  ) ## have to be ordered.
# need to note which features the variables correspond to.
RacePopulation <-
  get_acs(
    geography = "county",
    variables = RacevariableSets,
    survey = "acs5"
  )
RacePopulationMatrix <-
  matrix(RacePopulation$estimate, nrow = nCounty, byrow = TRUE)
colnames(RacePopulationMatrix) <- RacevariableSets
RaceGroupsProp <-
  RacePopulationMatrix[, 2:length(RacevariableSets)] / RacePopulationMatrix[, 1]
colnames(RaceGroupsProp) <- c(
  "Proportion of White",
  "Proportion of African American",
  "Proportion of Native",
  "Proportion of Asian",
  "Proportion of Pacific Native",
  "Proportion of Other Races"
)

## median age
MedianAge <-
  get_acs(
    geography = "county",
    variables = "B01002_001",
    survey = "acs5"
  )
MedianAgeEst <- MedianAge$estimate

## income
IncomeVariableSets <-
  c("B19101_001E", "B19113_001E", "B19127_001E") ## have to be ordered.
Incomes <-
  get_acs(
    geography = "county",
    variables = IncomeVariableSets,
    survey = "acs5"
  )
IncomesMatrix <-
  matrix(Incomes$estimate, nrow = nCounty, byrow = TRUE)
colnames(IncomesMatrix) <-
  c(
    "Family Income",
    "Median Family Income",
    "Aggregate Family Income"
  )
# IncomeVariableSets

## gini index
GiniIndex <-
  get_acs(
    geography = "county",
    variables = "B19083_001E",
    survey = "acs5"
  )
GiniIndexEst <- GiniIndex$estimate

## means of TRANSPORTATION TO WORK
TransVariables <-
  c("B08301_001E", "B08301_010E", "B08301_011E", "B08301_013E")
MeansTransport <-
  get_acs(
    geography = "county",
    variables = TransVariables,
    survey = "acs5"
  )
MeansTransportMatrix <-
  matrix(MeansTransport$estimate, nrow = nCounty, byrow = TRUE)
MeansTransportProp <-
  MeansTransportMatrix[, 2:length(TransVariables)] / MeansTransportMatrix[, 1]
colnames(MeansTransportProp) <-
  c(
    "Proportion of Public Transport",
    "Proportion of Bus",
    "Proportion of Subway"
  )

## Geographical mobility
MobilityVariables <-
  c(
    "B07001_001E",
    "B07001_033E",
    "B07001_049E",
    "B07001_065E",
    "B07001_081E"
  )
Mobility <-
  get_acs(
    geography = "county",
    variables = MobilityVariables,
    survey = "acs5"
  )
MobilityMatrix <-
  matrix(Mobility$estimate, nrow = nCounty, byrow = TRUE)
colnames(MobilityMatrix) <- MobilityVariables
MobilityProp <-
  MobilityMatrix[, 2:length(MobilityVariables)] / MobilityMatrix[, 1]
colnames(MobilityProp) <- c(
  "Proportion Moved Within Same County",
  "Proportion Moved From Different County",
  "Proportion Moved from Different State",
  "Proportion Moved From Abroad"
)

## health insurance
# private
nVariables <- 57
nGroups <- 9

healthInsVariablesPrivate <- rep("", nVariables)

for (i in 1:9) {
  healthInsVariablesPrivate[i] <- paste("B27002_00", i, "E", sep = "")
}

for (i in 10:nVariables) {
  healthInsVariablesPrivate[i] <- paste("B27002_0", i, "E", sep = "")
}

healthInsDataPrivate <-
  get_acs(
    geography = "county",
    variables = healthInsVariablesPrivate,
    survey = "acs5"
  )
healthInsDataMatrixPrivate <-
  matrix(healthInsDataPrivate$estimate,
    nrow = nCounty,
    byrow = TRUE
  )
healthInsPropPrivate <- matrix(0, nCounty, 1)
for (i in 1:nGroups) {
  healthInsPropPrivate <-
    healthInsPropPrivate + healthInsDataMatrixPrivate[, (3 * i) + 1]
  healthInsPropPrivate <-
    healthInsPropPrivate + healthInsDataMatrixPrivate[, 3 * (nGroups + i) + 2]
}
healthInsPropPrivate <-
  healthInsPropPrivate / healthInsDataMatrixPrivate[, 1]

# public
healthInsVariablesPublic <- rep("", nVariables)

for (i in 1:9) {
  healthInsVariablesPublic[i] <- paste("B27003_00", i, "E", sep = "")
}

for (i in 10:nVariables) {
  healthInsVariablesPublic[i] <- paste("B27003_0", i, "E", sep = "")
}

healthInsDataPublic <-
  get_acs(
    geography = "county",
    variables = healthInsVariablesPublic,
    survey = "acs5"
  )
healthInsDataMatrixPublic <-
  matrix(healthInsDataPublic$estimate,
    nrow = nCounty,
    byrow = TRUE
  )
healthInsPropPublic <- matrix(0, nCounty, 1)
for (i in 1:nGroups) {
  healthInsPropPublic <-
    healthInsPropPublic + healthInsDataMatrixPublic[, (3 * i) + 1]
  healthInsPropPublic <-
    healthInsPropPublic + healthInsDataMatrixPublic[, 3 * (nGroups + i) + 2]
}
healthInsPropPublic <-
  healthInsPropPublic / healthInsDataMatrixPublic[, 1]


### combine all the analysis features together
acs5Stats <- cbind(
  TotalPopEst, PopDensity, MedianAgeEst,
  IncomesMatrix,
  GiniIndexEst,
  MaleProp,
  RaceGroupsProp,
  MeansTransportProp,
  MobilityProp,
  healthInsPropPrivate, healthInsPropPublic
)

colnames(acs5Stats) <- c(
  "pop", "pop_density", "med_age",
  "family", "med_income", "aggr_income",
  "gini",
  "male",
  "whites", "african_americans", "natives",
  "asians", "pacifics", "other_races",
  "public_trans", "bus", "subway",
  "same_county", "diff_county",
  "diff_state", "abroad",
  "private_ins", "public_ins"
)

acs5Stats <- cbind(as.numeric(TotalPop$GEOID), acs5Stats)
# saveRDS(acs5Stats, "./data/census_full.rds")

colnames(acs5Stats)[1] <- "FIPS"
fips <- readRDS("./output/fips.rds")
acs5Stats <- as.data.frame(acs5Stats)
census <- as_tibble(left_join(data.frame(FIPS = fips), acs5Stats, by = "FIPS"))
# saveRDS(census, "./output/census.rds")

var_names <- c(
  "Total Population", "Population Density", "Median Age",
  "Number of Families", "Median Family Income", "Aggregate Family Income",
  "Gini Index",
  "Proportion of Male",
  "Proportion of Whites", "Proportion of African Americans", "Proportion of Natives",
  "Proportion of Asians", "Proportion of Pacific Natives", "Proportion of Other Races",
  "Proportion of Individuals who Used Public Transport", "Proportion of Individuals who Used Bus", "Proportion of Individuals who Used Subway",
  "Proportion who Moved within the Same County", "Proportion who Moved in from a Different County",
  "Proportion who Moved in from a Different State", "Proportion who Moved from Abroad",
  "Proportion with Private Health Insurance", "Proportion with Public Health Insurance"
)

# saveRDS(var_names, "./output/var_names.rds")

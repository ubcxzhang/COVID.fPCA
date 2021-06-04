# add lockdown data

library(tidyverse)
library(choroplethr)

data("df_pop_county")

PC1_score <- readRDS("./output/PC1_Score.rds")
census <- readRDS("./output/census.rds")
lc5 <- readRDS("./output/lc5.rds")

state_county <- read_csv("./data/1-31-21.csv") %>%
  filter(FIPS %in% df_pop_county$region) %>%
  filter(lc5$id.retain) %>%
  select(Province_State) %>%
  rename(state = Province_State)

day1 <- lc5$day1
state_lockdown <- read_csv("./data/lockdown.csv") %>%
  select(-Initial) %>%
  rename(state = Name)
state_lockdown$Lockdown_in_effect <- as.Date(state_lockdown$Lockdown_in_effect, tryFormats = c("%m/%d/%y"))

county_lockdown <- left_join(state_county, state_lockdown, by = "state")
county_lockdown <- mutate(county_lockdown, lockdown = as.numeric(difftime(county_lockdown$Lockdown_in_effect, as.Date("1/22/20", tryFormats = c("%m/%d/%y")), units = "days")) + 1 - day1)

lockdown_census <- cbind(county_lockdown$lockdown, census[, -1]) %>%
  rename(lockdown = "county_lockdown$lockdown") %>%
  as_tibble()

# saveRDS(lockdown_census, "./output/lockdown_census.rds")

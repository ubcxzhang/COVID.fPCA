# add npi data

library(tidyverse)
library(choroplethr)
library(usdata)

process_date_flag <- function(df, threshold, flag) {
  states <- data.frame(state = unique(df$state))
  name <- names(df)[3]
  df2 <- df %>%
    group_by(state) %>%
    filter(!!sym(names(.)[[3]]) >= threshold & !!sym(names(.)[[4]]) >= flag) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-c(3, 4)) %>%
    rename(!!name := Date)

  if (name == "E1_Income support") {
    df2 <- df %>%
      group_by(state) %>%
      filter(!!sym(names(.)[[3]]) >= threshold & !!sym(names(.)[[4]]) >= flag & !!sym(names(.)[[2]]) > as.Date("2020-01-27")) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(-c(3, 4)) %>%
      rename(!!name := Date)
  }

  df <- df2


  left_join(state_day1, df, by = "state") %>%
    mutate(diff = as.numeric(difftime(!!sym(names(.)[[3]]), as.Date("1/22/20", tryFormats = c("%m/%d/%y")), units = "days")) + 1 - day1) %>%
    select(diff) %>%
    rename(!!name := diff)
}

process_date <- function(df, threshold) {
  states <- data.frame(state = unique(df$state))
  name <- names(df)[3]
  df <- df %>%
    group_by(state) %>%
    filter(!!sym(names(.)[[3]]) >= threshold) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(-3) %>%
    rename(!!name := Date)

  left_join(state_day1, df, by = "state") %>%
    mutate(diff = as.numeric(difftime(!!sym(names(.)[[3]]), as.Date("1/22/20", tryFormats = c("%m/%d/%y")), units = "days")) + 1 - day1) %>%
    select(diff) %>%
    rename(!!name := diff)
}

process_sum <- function(df) {
  name <- names(df)[2]
  df <- df %>%
    group_by(state) %>%
    summarise(sum = ifelse(sum(is.na(!!sym(names(.)[[2]]))) == n(), NA, sum(!!sym(names(.)[[2]]), na.rm = TRUE)))

  left_join(state_day1, df, by = "state") %>%
    select(-(1:2)) %>%
    rename(!!name := sum)
}


data("df_pop_county")
lc5 <- readRDS("./output/lc5.rds")
PC1_score <- readRDS("./output/PC1_score.rds")
PC2_score <- readRDS("./output/PC2_score.rds")
var_names <- readRDS("./output/var_names.rds")
fpca <- readRDS("./output/fpca.rds")

cgrt_raw <- read_csv("./data/OxCGRT_latest.csv", col_types = cols(RegionName = col_character(), RegionCode = col_character(), Date = col_date("%Y%m%d")))

cgrt <- cgrt_raw %>%
  filter(CountryCode == "USA" & Jurisdiction == "STATE_TOTAL") %>%
  select(-c(1, 2, 3, 5, 39:51)) %>%
  mutate(state = str_sub(RegionCode, 4), .before = RegionCode, .keep = "unused")

state_day1 <- read_csv("./data/1-31-21.csv") %>%
  filter(FIPS %in% df_pop_county$region) %>%
  filter(lc5$id.retain) %>%
  select(Province_State) %>%
  rename(state = Province_State) %>%
  transmute(state = state2abbr(state)) %>%
  bind_cols(lc5$day1) %>%
  rename(day1 = ...2)

npi <- data.frame(
  `E1_Income support` = process_date_flag(select(cgrt, state, Date, `E1_Income support`, `E1_Flag`), 1, 1),
  `E2_Debt/contract relief` = process_date(select(cgrt, state, Date, `E2_Debt/contract relief`), 1),
  `E3_Fiscal measures` = process_sum(select(cgrt, state, `E3_Fiscal measures`)),
  `E4_International support` = process_sum(select(cgrt, state, `E4_International support`)),
  `H1_Public information campaigns` = process_date_flag(select(cgrt, state, Date, `H1_Public information campaigns`, `H1_Flag`), 1, 1),
  `H2_Testing policy` = process_date(select(cgrt, state, Date, `H2_Testing policy`), 1),
  `H3_Contact tracing` = process_date(select(cgrt, state, Date, `H3_Contact tracing`), 1),
  `H4_Emergency investment in healthcare` = process_sum(select(cgrt, state, `H4_Emergency investment in healthcare`)),
  `H5_Investment in vaccines` = process_sum(select(cgrt, state, `H5_Investment in vaccines`)),
  `H6_Facial Coverings` = process_date_flag(select(cgrt, state, Date, `H6_Facial Coverings`, `H6_Flag`), 2, 1),
  `H7_Vaccination Policy` = process_date_flag(select(cgrt, state, Date, `H7_Vaccination policy`, `H7_Flag`), 1, 1),
  `H8_Protection of elderly people` = process_date_flag(select(cgrt, state, Date, `H8_Protection of elderly people`, `H8_Flag`), 2, 1)
)
npi <- npi %>% mutate(
  H6_Facial.Coverings = ifelse(is.na(H6_Facial.Coverings), max(H6_Facial.Coverings, na.rm = TRUE), H6_Facial.Coverings),
  H8_Protection.of.elderly.people = ifelse(is.na(H8_Protection.of.elderly.people), max(H8_Protection.of.elderly.people, na.rm = TRUE), H8_Protection.of.elderly.people)
)

complex_lockdown_census <- readRDS("./output/complex_lockdown_census.rds")

combined_complex_lockdown_census <- bind_cols(complex_lockdown_census, npi) %>%
  select(-c(
    bus, subway, aggr_income, family, diff_county, diff_state, abroad,
    other_races, pacifics,
    E3_Fiscal.measures, E4_International.support,
    H4_Emergency.investment.in.healthcare, H5_Investment.in.vaccines,
    E1_Income.support
  ))

# saveRDS(combined_complex_lockdown_census, "./output/combined_complex_lockdown_census.rds")
# saveRDS(combined_complex_lockdown_census[,-(2:4)], "./output/combined_lockdown_census.rds")

# other lockdowns

library(tidyverse)
library(choroplethr)
library(segmented)
library(RColorBrewer)
PC1_score <- readRDS("./output/PC1_Score.rds")
census <- readRDS("./output/census.rds")
lc5 <- readRDS("./output/lc5.rds")
palSet1 <- brewer.pal(9, "Set1")
data("df_pop_county")


add_lockdown <- function(state_lockdown, name) {
  state_county <- read_csv("./data/1-31-21.csv") %>%
    filter(FIPS %in% df_pop_county$region) %>%
    filter(lc5$id.retain) %>%
    select(Province_State) %>%
    rename(state = Province_State)


  day1 <- lc5$day1
  state_lockdown <- state_lockdown %>%
    select(-Initial) %>%
    rename(state = Name)


  county_lockdown <- left_join(state_county, state_lockdown, by = "state")
  county_lockdown <- mutate(county_lockdown, lockdown = as.numeric(difftime(county_lockdown$Lockdown_in_effect, as.Date("1/22/20", tryFormats = c("%m/%d/%y")), units = "days")) + 1 - day1)


  lockdown_census <- cbind(county_lockdown$lockdown, census[, -1]) %>%
    rename(lockdown = "county_lockdown$lockdown")

  saveRDS(as_tibble(lockdown_census), paste0("./output/lockdown_census_", name, ".rds"))
}

calc_changing_point <- function(name) {
  lockdown_census <- readRDS(paste0("./output/lockdown_census_", name, ".rds"))


  yForSegmented <- PC1_score
  xForSegmented <- lockdown_census$lockdown

  if (name == "Restrictions on gatherings") {
    yForSegmented <- yForSegmented[xForSegmented < 30]
    xForSegmented <- xForSegmented[xForSegmented < 30]
  } else if (name == "Restrictions on internal movement") {
    yForSegmented <- yForSegmented[xForSegmented < 60]
    xForSegmented <- xForSegmented[xForSegmented < 60]
  }



  lin.mod <- lm(yForSegmented ~ xForSegmented)
  segmented.mod <- segmented(lin.mod, seg.Z = ~xForSegmented)

  best_beta <- summary.segmented(segmented.mod)$psi[1, 2]

  fit <- broken.line(segmented.mod)$fit

  if (name %in% c("Restrictions on gatherings", "Restrictions on internal movement")) {
    data <- data.frame(x = lockdown_census$lockdown[!is.na(lockdown_census$lockdown)], y = PC1_score[!is.na(lockdown_census$lockdown)])
    data_fit <- data.frame(x = xForSegmented[!is.na(xForSegmented)], fit = fit)

    p1 <- ggplot(data, aes(x = x, y = y)) +
      geom_point(color = palSet1[2], size = 0.5) +
      geom_line(data = data_fit, aes(x = x, y = fit), color = palSet1[1], size = 1) +
      geom_vline(xintercept = best_beta, color = palSet1[3], size = 1) +
      labs(title = paste0(name, ": segmented"), subtitle = paste0("Breakpoint = ", signif(best_beta, 5)), x = "Lockdown date since the first day with 5 or more cases", y = "First FPC Score") +
      theme(
        text = element_text(size = 14), panel.background = element_rect(
          fill = "white",
          color = "black",
          size = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  } else {
    data <- data.frame(x = xForSegmented[!is.na(xForSegmented)], y = yForSegmented[!is.na(xForSegmented)], fit = fit)

    p1 <- ggplot(data, aes(x = x, y = y)) +
      geom_point(color = palSet1[2], size = 0.5) +
      geom_line(aes(x = x, y = fit), color = palSet1[1], size = 1) +
      geom_vline(xintercept = best_beta, color = palSet1[3], size = 1) +
      labs(title = paste0(name, ": segmented"), subtitle = paste0("Breakpoint = ", signif(best_beta, 5)), x = "Lockdown date since the first day with 5 or more cases", y = "First FPC Score") +
      theme(
        text = element_text(size = 14), panel.background = element_rect(
          fill = "white",
          color = "black",
          size = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }

  ggsave(paste0("lockdown_segmented_", name, ".pdf"), p1, "pdf", "./figs", width = 9, height = 6.5, units = "in")
}


cgrt_raw <- read_csv("./data/OxCGRT_latest.csv", col_types = cols(RegionName = col_character(), RegionCode = col_character(), Date = col_date("%Y%m%d")))


cgrt <- cgrt_raw %>%
  filter(CountryCode == "USA" & Jurisdiction == "STATE_TOTAL")


dates <- data.frame(RegionName = unique(cgrt$RegionName), Initial = unique(cgrt$RegionCode))


for (i in seq(7, 19, 2)) {
  if (i == 13) {
    threshold <- 1
  } else {
    threshold <- 2
  }
  date <- cgrt %>%
    filter(across(i, ~ . >= threshold) & across(i + 1, ~ . >= 1)) %>%
    group_by(RegionName) %>%
    filter(row_number() == 1) %>%
    select(Date)
  dates <- left_join(dates, date, by = "RegionName")
}

dates <- dates %>%
  mutate(Name = str_replace(RegionName, "Washington DC", "District of Columbia")) %>%
  relocate(Name, Initial) %>%
  select(-RegionName)

names(dates) <- c("Name", "Initial", paste0("C", 1:7))


dates <- dates %>% select(-c(C5, C6))

names(dates) <- c("Name", "Initial", "School closing", "Workplace closing", "Cancel public events", "Restrictions on gatherings", "Restrictions on internal movement")

for (i in c(3:7)) {
  data <- bind_cols(dates[, 1:2], dates[i])
  name <- names(data)[3]
  names(data)[3] <- "Lockdown_in_effect"
  add_lockdown(data, name)
  calc_changing_point(name)
}

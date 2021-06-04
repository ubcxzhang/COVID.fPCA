# lockdown groups

library(tidyverse)
library(fdapace)

complex_lockdown_census <- readRDS("./output/combined_complex_lockdown_census.rds")
best_beta <- readRDS("./output/best_beta.rds")
fpca <- readRDS("./output/fpca.rds")
var_names <- readRDS("./output/var_names.rds")

complex_lockdown_groups_census <- complex_lockdown_census %>%
  mutate(lockdown_group = case_when(
    is.na(lockdown) ~ "No",
    lockdown < best_beta ~ "Early",
    lockdown >= best_beta ~ "Late"
  ))

early <- filter(complex_lockdown_groups_census, lockdown_group == "Early")
(nrow(early))
late <- filter(complex_lockdown_groups_census, lockdown_group == "Late")
(nrow(late))

for (i in 5:(ncol(complex_lockdown_groups_census) - 1)) {
  print(i)
  print(wilcox.test(pull(early, i), pull(late, i))$p.value < 0.05)
}

nVars <- 21
group_stats <- matrix(NA, nVars, 4)

group_stats[, 1] <- as.vector(colMeans(filter(complex_lockdown_groups_census, lockdown_group == "Early")[-c(1:4, ncol(complex_lockdown_groups_census))], na.rm = TRUE))
group_stats[, 2] <- apply(filter(complex_lockdown_groups_census, lockdown_group == "Early")[-c(1:4, ncol(complex_lockdown_groups_census))], 2, sd, na.rm = TRUE)

group_stats[, 3] <- as.vector(colMeans(filter(complex_lockdown_groups_census, lockdown_group == "Late")[-c(1:4, ncol(complex_lockdown_groups_census))], na.rm = TRUE))
group_stats[, 4] <- apply(filter(complex_lockdown_groups_census, lockdown_group == "Late")[-c(1:4, ncol(complex_lockdown_groups_census))], 2, sd, na.rm = TRUE)

group_stats <- as.data.frame(group_stats)

group_stats <- bind_cols(var_names, group_stats)
colnames(group_stats) <- c("variable", "mean_early", "sd_early", "mean_late", "sd_late")

group_stats[c(1, 4), -1] <- 1e-3 * group_stats[c(1, 4), -1]
group_stats[6:14, -1] <- 100 * group_stats[6:14, -1]

group_stats[, -1] <- signif(group_stats[-1], 3)

group_stats[] <- lapply(group_stats, as.character)

group_stats$meansd_early <- paste0(group_stats$mean_early, "\u00B1", group_stats$sd_early)
group_stats$meansd_late <- paste0(group_stats$mean_late, "\u00B1", group_stats$sd_late)

# write_csv(group_stats, "./output/group_stats.csv")

fitted_data <- fitted(fpca)
fitted_early <- fitted_data[complex_lockdown_groups_census$lockdown_group == "Early", ]
fitted_late <- fitted_data[complex_lockdown_groups_census$lockdown_group == "Late", ]

mean_early <- apply(fitted_early, 2, mean) / log(10)
mean_late <- apply(fitted_late, 2, mean) / log(10)

# draw grouped mean curve with IQR 6.5 * 9
qt_early <- apply(fitted_early, 2, quantile) / log(10)
qt_late <- apply(fitted_late, 2, quantile) / log(10)

plot(x = fpca$workGrid, y = fpca$mu / log(10), type = "l", col = "black", main = NULL, xlab = "Time in days", ylab = "Number of cases", ylim = c(0, 6.5), yaxt = "n", lty = 3)
polygon(x = c(fpca$workGrid, rev(fpca$workGrid)), y = c(qt_early[4, ], rev(qt_early[2, ])), col = adjustcolor("blue", alpha.f = 0.05), border = NA)
polygon(x = c(fpca$workGrid, rev(fpca$workGrid)), y = c(qt_late[4, ], rev(qt_late[2, ])), col = adjustcolor("red", alpha.f = 0.05), border = NA)
lines(x = fpca$workGrid, y = mean_early, col = "blue", lwd = 2)
lines(x = fpca$workGrid, y = mean_late, col = "red", lwd = 2)
lines(x = fpca$workGrid, y = fpca$mu / log(10), col = "black", lty = 3)
axis(2, at = 0:6, labels = c(expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6)))
legend("topleft", legend = c("all counties", "counties with lockdown before the inflection point", "counties with lockdown after the inflection point"), pch = c(NA, NA, NA), lty = c(3, 1, 1), col = c("black", "blue", "red"), lwd = c(1, 2, 2), cex = 0.8)

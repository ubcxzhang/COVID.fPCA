# segmented regression

library(tidyverse)
library(segmented)
library(RColorBrewer)

palSet1 <- brewer.pal(9, "Set1")

PC1_score <- readRDS("./output/PC1_Score.rds")
lockdown_census <- readRDS("./output/lockdown_census.rds")

calculate_best_beta <- function(low, high, step) {
  logLik <- matrix(ncol = 2)

  for (beta in seq(low, high, step)) {
    lockdown_census <- lockdown_census %>%
      mutate(if_lockdown = ifelse(!is.na(lockdown), 1, 0)) %>%
      mutate(lockdown_before = ifelse(if_lockdown != 0, lockdown, 0)) %>%
      mutate(lockdown_after = ifelse(if_lockdown != 0 & lockdown_before >= beta, lockdown_before - beta, 0))

    fit <- lm(PC1_score ~ lockdown_census$if_lockdown + lockdown_census$lockdown_before + lockdown_census$lockdown_after)

    (summary(fit))
    logLik <- rbind(logLik, c(beta, logLik(fit)))
  }

  plot(logLik, type = "l", main = "Log Likelihood of Multi Linear Regression of Complex Lockdown Model", xlab = "Changing Point", ylab = "Log Likelihood")

  return(logLik[which.max(logLik[, 2]), 1])
}

(best_beta <- calculate_best_beta(-8, -7, 0.01))
# saveRDS(best_beta, "./output/best_beta.rds")

yForSegmented <- PC1_score
xForSegmented <- lockdown_census$lockdown
lin.mod <- lm(yForSegmented ~ xForSegmented)
segmented.mod <- segmented(lin.mod, seg.Z = ~xForSegmented)

(best_beta <- summary.segmented(segmented.mod)$psi[1, 2])

complex_lockdown_census <- lockdown_census %>%
  mutate(
    if_lockdown = ifelse(!is.na(lockdown), 1, 0),
    lockdown_before = ifelse(if_lockdown != 0, lockdown, 0),
    lockdown_after = ifelse(if_lockdown != 0 & lockdown_before >= best_beta, lockdown_before - best_beta, 0)
  ) %>%
  relocate(if_lockdown, lockdown_before, lockdown_after, .after = lockdown)
# saveRDS(complex_lockdown_census, "./output/complex_lockdown_census.rds")

fit <- broken.line(segmented.mod)$fit
df <- data.frame(x = xForSegmented[!is.na(xForSegmented)], y = yForSegmented[!is.na(xForSegmented)], fit = fit)

p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(color = palSet1[2], size = 0.5) +
  geom_line(aes(x = x, y = fit), color = palSet1[1], size = 1) +
  geom_vline(xintercept = best_beta, color = palSet1[3], size = 1) +
  labs(title = "Lockdown: segmented", subtitle = paste0("Breakpoint = ", signif(best_beta, 5)), x = "Lockdown date since the first day with 5 or more cases", y = "First FPC Score") +
  theme(
    text = element_text(size = 14), panel.background = element_rect(
      fill = "white",
      color = "black",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ggsave(paste0("lockdown_segmented.pdf"), p1, "pdf", "./figs/", width = 9, height = 6.5, units = "in")

library(tidyverse)
library(segmented)
library(RColorBrewer)

palset_1 <- brewer.pal(9, "Set1")

# calculate best beta using maximum likelihood
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

PC1_score <- readRDS("./data/PC1_Score.rds")
lockdown_census <- readRDS("./data/lockdown_census.rds")

(best_beta <- calculate_best_beta(-8, -7, 0.01))
# saveRDS(best_beta, "./data/best_beta.rds")

# segmented package gives the same result
yForSegmented <- PC1_score
xForSegmented <- lockdown_census$lockdown
lin.mod <- lm(yForSegmented ~ xForSegmented)
segmented.mod <- segmented(lin.mod, seg.Z = ~xForSegmented)
plot(segmented.mod)
(summary.segmented(segmented.mod))
(best_beta = summary.segmented(segmented.mod)$psi[1, 2]) # -7.6992

# calulate segmented lockdown variables
complex_lockdown_census <- lockdown_census %>%
  mutate(if_lockdown = ifelse(!is.na(lockdown), 1, 0), 
         lockdown_before = ifelse(if_lockdown != 0, lockdown, 0), 
         lockdown_after = ifelse(if_lockdown != 0 & lockdown_before >= best_beta, lockdown_before - best_beta, 0)) %>%
  relocate(if_lockdown, lockdown_before, lockdown_after, .after = lockdown)
# saveRDS(complex_lockdown_census, "./data/complex_lockdown_census.rds")

summary(lm(PC1_score ~ complex_lockdown_census$if_lockdown + complex_lockdown_census$lockdown_before + complex_lockdown_census$lockdown_after))

# plot segmented lockdown
fit <- lm(PC1_score ~ complex_lockdown_census$if_lockdown + complex_lockdown_census$lockdown_before + complex_lockdown_census$lockdown_after)
coefs <- as.vector(coef(summary(fit))[, 1])
left_most_y <- coefs[1] + coefs[2] + coefs[3] * min(na.omit(complex_lockdown_census$lockdown))
changing_point_y <- coefs[1] + coefs[2] + coefs[3] * best_beta
right_most_y <- coefs[1] + coefs[2] + coefs[3] * best_beta + coefs[4] * (max(na.omit(complex_lockdown_census$lockdown)) - best_beta)

p1 <- ggplot(data = data.frame(x = complex_lockdown_census$lockdown, y = PC1_score), aes(x = x, y = y)) +
  labs(title = "Lockdown: segmented", subtitle = paste0("Breakpoint = ", signif(best_beta, 5)), x = "Lockdown date since the first day with 5 or more cases", y = "First FPC Score") +
  geom_point(color = palset_1[2], size = 0.5) +
  geom_line(data = data.frame(x = c(min(na.omit(complex_lockdown_census$lockdown)), best_beta), y = c(left_most_y, changing_point_y)), aes(x = x, y = y), color = palset_1[1], size = 1) +
  geom_line(data = data.frame(x = c(best_beta, max(na.omit(complex_lockdown_census$lockdown))), y = c(changing_point_y, right_most_y)), aes(x = x, y = y), color = palset_1[1], size = 1) +
  geom_vline(xintercept = best_beta, color = palset_1[3], size = 1) +
  ylim(-80, 80) + 
  theme(text = element_text(size=14), panel.background = element_rect(
    fill = "white",
    colour = "black",
    size = 0.5, linetype = "solid"
  ),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

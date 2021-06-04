# marginal regression

library(tidyverse)
library(RColorBrewer)
library(gridExtra)

fit_marginal <- function(n) {
  acs5COVIDLm <- rep(0, ncol(lockdown_census_scale))
  acs5COVIDLmPv <- rep(0, ncol(lockdown_census_scale))
  acs5COVIDLmAdjRSq <- rep(0, ncol(lockdown_census_scale))

  for (i in 1:ncol(lockdown_census_scale)) {
    lm.fit <- lm(PC_scores_scale[, n] ~ lockdown_census_scale[, i])
    fit_summary <- summary(lm.fit)
    coefficients <- coef(fit_summary)
    acs5COVIDLm[i] <- coefficients[2, 1]
    acs5COVIDLmPv[i] <- coefficients[2, 4]
    acs5COVIDLmAdjRSq[i] <- fit_summary$adj.r.squared
  }

  data.frame(var_name = var_names, coef = acs5COVIDLm, p_value = acs5COVIDLmPv, adj_rsq = acs5COVIDLmAdjRSq)
}

remove_outliers <- function(vector) {
  lower <- quantile(vector, na.rm = TRUE)[2] - 1.5 * IQR(vector, na.rm = TRUE)
  upper <- quantile(vector, na.rm = TRUE)[4] + 1.5 * IQR(vector, na.rm = TRUE)
  vector <- replace(vector, vector < lower | vector > upper, NA)
}

plot_marginal <- function(y, x) {
  for (i in 0:1) {
    pdf(paste0("./figs/marginal/", i + 1, ".pdf"), width = 9.75, height = 12, pointsize = 4)
    pl <- vector(mode = "list", length = 6)
    for (j in 1:6) {
      var_ind <- i * 6 + j
      var_name <- var_names[var_ind]
      df <- data.frame(y = y, x = lockdown_census_scale[, var_ind], x_no = remove_outliers(lockdown_census_scale[, var_ind]))
      pl[[j]] <- ggplot(df) +
        geom_point(aes(x_no, y), color = pal_set_1[2], size = 0.05) +
        geom_smooth(aes(x, y), method = "lm", formula = y ~ x, se = FALSE, color = pal_set_1[1]) +
        xlim(min(df$x_no, na.rm = TRUE), max(df$x_no, na.rm = TRUE)) +
        ylim(min(df$y[!is.na(df$x_no)], na.rm = TRUE), max(df$y[!is.na(df$x_no)], na.rm = TRUE)) +
        labs(title = paste("Linear Regression of", var_name), x = var_name, y = "First FPC Score")
      if (nchar(paste("Linear Regression of", var_name)) > 50) {
        pl[[j]] <- pl[[j]] + theme(plot.title = element_text(size = 9))
      }
    }
    do.call(grid.arrange, pl)
    dev.off()
  }
}

pal_set_1 <- brewer.pal(9, "Set1")

lockdown_census <- readRDS("./output/combined_lockdown_census.rds")
var_names <- readRDS("./output/var_names.rds")
var_names <- c("Lockdown", var_names)
PC1_score <- readRDS("./output/PC1_score.rds")
PC2_score <- readRDS("./output/PC2_score.rds")
PC_scores <- cbind(PC1_score, PC2_score)

lockdown_census_scale <- as.matrix(lockdown_census)
PC_scores_scale <- PC_scores

m1 <- fit_marginal(1)
# saveRDS(m1, "./output/marginal.rds")
m1[, -1] <- signif(m1[, -1], 5)
# write_csv(m1, "./output/marginal.csv")

# plot_marginal(PC_scores_scale[, 1], lockdown_census_scale)

y <- PC_scores_scale[, 1]
x <- lockdown_census_scale
pdf(paste0("./figs/marginal/", 3, ".pdf"), width = 9.75, height = 8, pointsize = 4)
pl <- vector(mode = "list", length = 2)
for (j in 1:3) {
  var_ind <- j + 12
  var_name <- var_names[var_ind]
  df <- data.frame(y = y, x = lockdown_census_scale[, var_ind], x_no = remove_outliers(lockdown_census_scale[, var_ind]))
  pl[[j]] <- ggplot(df) +
    geom_point(aes(x_no, y), color = pal_set_1[2], size = 0.05) +
    geom_smooth(aes(x, y), method = "lm", formula = y ~ x, se = FALSE, color = pal_set_1[1]) +
    xlim(min(df$x_no, na.rm = TRUE), max(df$x_no, na.rm = TRUE)) +
    ylim(min(df$y[!is.na(df$x_no)], na.rm = TRUE), max(df$y[!is.na(df$x_no)], na.rm = TRUE)) +
    labs(title = paste("Linear Regression of", var_name), x = var_name, y = "First FPC Score")
  if (nchar(paste("Linear Regression of", var_name)) > 50) {
    pl[[j]] <- pl[[j]] + theme(plot.title = element_text(size = 9))
  }
}
do.call(grid.arrange, c(pl, ncol = 2))
dev.off()

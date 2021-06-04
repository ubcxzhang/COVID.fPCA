# bootstrap

library(tidyverse)
library(caret)
library(glmnet)
library(boot)
library(ggpubr)
library(reshape2)

heatmap <- function(df) {
  cor <- cor(df[complete.cases(df), ])

  get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  upper_tri <- get_upper_tri(cor)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)

  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "Pearson\nCorrelation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45, vjust = 1,
        size = 8, hjust = 1
      ), axis.text.y = element_text(
        size = 8
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_fixed()
}

bootstrap <- function(df, pc, n, iter) {
  elnetData <- as.matrix(cbind(pc[, n], df))
  PC_col_name <- paste0("PC", n, "_Score")
  colnames(elnetData)[1] <- PC_col_name
  elnetData <- elnetData[complete.cases(elnetData), ]
  folds <- createFolds(elnetData[, 1], 10, returnTrain = TRUE)
  cv_10 <- trainControl(method = "cv", index = folds)
  form <- as.formula(paste(PC_col_name, "~."))

  foo <- function(data, indices) {
    sample <- data[indices, ]
    elnet <- train(
      form,
      data = sample,
      method = "glmnet",
      trControl = cv_10,
      tuneLength = 10
    )
    x <- sample[, -1]
    y <- sample[, 1]
    glmfit <- glmnet(x, y, alpha = elnet$bestTune$alpha, lambda = elnet$bestTune$lambda)
    coefs <- as.matrix(coef(glmfit))
    rsq <- glmfit$dev.ratio
    adj_rsq <- 1 - (1 - rsq) * (nrow(x) - 1) / (nrow(x) - ncol(x) - 1)
    temp2 <- coefs[3]
    temp3 <- coefs[4]
    coefs[4] <- coefs[3] + coefs[4]
    return(c(coefs[-1], adj_rsq, temp2, temp3))
  }

  tot_rep <- iter
  myBootstrap <- boot(elnetData, foo, R = tot_rep, parallel = "multicore", ncpus = 10)

  return(myBootstrap)
}

PC1_score <- readRDS("./output/PC1_score.rds")
PC2_score <- readRDS("./output/PC2_score.rds")
var_names <- readRDS("./output/var_names.rds")
fpca <- readRDS("./output/fpca.rds")
combined_complex_lockdown_census <- readRDS("./output/combined_complex_lockdown_census.rds")

PC_scores <- cbind(PC1_score, PC2_score)

temp <- select(combined_complex_lockdown_census, -lockdown)
df <- combined_complex_lockdown_census[-(2:4)]
names(df) <- c("Lockdown", var_names)
heatmap(df)

complex_lockdown_census_decorr <- temp

complex_lockdown_census_decorr_scale <- scale(complex_lockdown_census_decorr)
PC_scores_scale <- PC_scores

t1 <- Sys.time()
print(t1)
set.seed(0)
boot1 <- bootstrap(complex_lockdown_census_decorr_scale, PC_scores_scale, 1, 1000)
print(Sys.time() - t1)
# saveRDS(boot1, "./output/boot1.rds")

(boot_rsq <- 2 * boot1$t0[length(boot1$t0) - 2] - mean(boot1$t[, length(boot1$t0) - 2]))
# saveRDS(boot_rsq, "./output/boot_rsq.rds")

# build ci
ci <- vector()
for (i in 1:ncol(complex_lockdown_census_decorr_scale)) {
  m <- 2 * boot1$t0[i] - mean(boot1$t[, i])
  s <- sd(boot1$t[, i])
  ci <- rbind(ci, c(m, boot.ci(boot1, type = "norm", index = i)$normal[2:3]))
}

ci <- data.frame(
  Variable = c(
    "Lockdown Indicator",
    "Lockdown Slope before Inflection Point",
    "Lockdown Slope after Inflection Point",
    var_names
  ),
  low = ci[, 2],
  mean = ci[, 1],
  high = ci[, 3]
)

xlab <- ci$Variable
xlab <- factor(xlab, levels = xlab)
xlab <- fct_rev(xlab)

# plot confidence intervals 7.5 * 9 portrait
p1 <- ggplot() +
  geom_pointrange(ci, mapping = aes(x = xlab, y = mean, ymin = low, ymax = high, col = low > 0 | high < 0)) +
  scale_colour_manual(values = setNames(c("blue", "grey"), c(T, F))) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Coefficient", title = "Confidence Interval of Elastic Net Coefficients") +
  theme(
    legend.position = "none",
    panel.background = element_rect(
      fill = "white",
      colour = "black",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

p1
# ggsave(paste0("ci.pdf"), p1, "pdf", "./figs/", width = 7.5, height = 9, units = "in")

(mean2 <- 2 * boot1$t0[length(boot1$t0) - 1] - mean(boot1$t[, length(boot1$t0) - 1]))
(mean3 <- 2 * boot1$t0[length(boot1$t0)] - mean(boot1$t[, length(boot1$t0)]))
(sd2 <- sd(combined_complex_lockdown_census$lockdown_before))
(sd3 <- sd(combined_complex_lockdown_census$lockdown_after))

(slope_after <- mean2 / sd2 + mean3 / sd3)
(rate_day <- (exp(c(min(fpca$phi[, 1]), max(fpca$phi[, 1]), mean(fpca$phi[, 1])) * slope_after) - 1) * 100)
(rate_week <- (exp(c(min(fpca$phi[, 1]), max(fpca$phi[, 1]), mean(fpca$phi[, 1])) * 7 * slope_after) - 1) * 100)

library(tidyverse)
library(caret)
library(glmnet)
library(boot)
library(ggpubr)

# elastic net to get r-sqr
elnet <- function(df, pc, n) {
  # elastic net
  elnetData <- as.matrix(cbind(pc[, n], df))
  PC_col_name <- paste0("PC", n, "_Score")
  colnames(elnetData)[1] <- PC_col_name
  elnetData <- elnetData[complete.cases(elnetData), ]
  cv_10 <- trainControl(method = "cv", number = 10)
  form <- as.formula(paste(PC_col_name, "~."))

  # get the best alpha and lambda
  elnet <- train(
    form,
    data = elnetData,
    method = "glmnet",
    trControl = cv_10,
    tuneLength = 10
  )

  # use the best tuned parameters to fit the model
  x <- elnetData[, -1]
  y <- elnetData[, 1]
  glmfit <- glmnet(x, y, alpha = elnet$bestTune$alpha, lambda = elnet$bestTune$lambda)

  rsq <- glmfit$dev.ratio
  adj_rsq <- 1 - (1 - rsq) * (nrow(x) - 1) / (nrow(x) - ncol(x) - 1)

  print(paste("alpha = ", elnet$bestTune$alpha, " ", "lambda = ", elnet$bestTune$lambda, sep = ""))
  # tmp_coeffs <- coef(glmfit)
  coefs <- as.matrix(coef(glmfit))
  # coefs = data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  print(coefs)
  print(paste("adj_rsq = ", adj_rsq, sep = ""))
  result <- list("alpha" = elnet$bestTune$alpha, "lambda" = elnet$bestTune$lambda, "coefs" = coefs, "adj_rsq" = adj_rsq, "elnet" = glmfit)
}

# bootstrap to get confidence intervals
bootstrap <- function(df, pc, n, iter) {
  # elastic net
  elnetData <- as.matrix(cbind(pc[, n], df))
  PC_col_name <- paste0("PC", n, "_Score")
  colnames(elnetData)[1] <- PC_col_name
  elnetData <- elnetData[complete.cases(elnetData), ]
  folds = createFolds(elnetData[,1], 10, returnTrain = TRUE)
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
    # use the best tuned parameters to fit the model
    x <- sample[, -1]
    y <- sample[, 1]
    glmfit <- glmnet(x, y, alpha = elnet$bestTune$alpha, lambda = elnet$bestTune$lambda)
    coefs <- as.matrix(coef(glmfit))
    rsq <- glmfit$dev.ratio
    adj_rsq <- 1 - (1 - rsq) * (nrow(x) - 1) / (nrow(x) - ncol(x) - 1)
    return (c(coefs, adj_rsq))
  }

  tot_rep <- iter
  myBootstrap <- boot(elnetData, foo, R = tot_rep, parallel = "multicore", ncpus = 10)

  return (myBootstrap)
}

# load data
complex_lockdown_census <- readRDS("./data/complex_lockdown_census.rds")
PC1_score <- readRDS("./data/PC1_score.rds")
PC2_score <- readRDS("./data/PC2_score.rds")
var_names <- readRDS("./data/var_names.rds")

PC_scores <- cbind(PC1_score, PC2_score)

# remove correlated variables
complex_lockdown_census_decorr <- select(temp, -c(family, aggr_income, public_trans, subway, private_ins, public_ins, other_races))

# scale data
complex_lockdown_census_decorr_scale <- scale(complex_lockdown_census_decorr)
PC_scores_scale <- scale(PC_scores)

# elasticnet and rsq
set.seed(0)
elnet1 <- elnet(complex_lockdown_census_decorr_scale, PC_scores_scale, 1)
# write.csv(as.matrix(coef(elnet1$elnet$finalModel, elnet1$elnet$bestTune$lambda)), "data/elnet1.csv")
# saveRDS(elnet1, "data/elnet1.rds")

# bootstrap to get confidence intervals
t1 = Sys.time()
set.seed(0)
boot1 <- bootstrap(complex_lockdown_census_decorr_scale, PC_scores_scale, 1, 1000)
print(Sys.time() - t1)
# saveRDS(boot1, "./data/boot1.rds")

# save bootstrap rsq
boot_rsq = mean(boot1$t[,21])
# saveRDS(boot_rsq, "./data/boot_rsq.rds")

# get confidence intervals
ci <- vector()
for (i in 1:ncol(complex_lockdown_census_decorr_scale)) {
  ci <- rbind(ci, c(mean(boot1$t[, i + 1]), boot.ci(boot1, index = i + 1, type = "norm")$norm[2:3]))
}

ci = data.frame(Variable = c(
  "Lockdown Indicator",
  "Lockdown Slope before Inflection Point",
  "Lockdown Slope after Inflection Point",
  var_names[-c(4, 6, 15, 17, 22, 23, 14)]
  ),
  low = ci[,2],
  mean = ci[,1],
  high = ci[,3]
)

# combine x2 and x3 to calculate slope
ci[3, -1] <- ci[2, -1] + ci[3, -1]

# save ci as table
ci_csv = cbind(Variable = ci[,1], signif(ci[,2:4], 3))
# write_csv(ci_csv, "./data/ci.csv")

## ci plot
# calculate factor used as axis labels
xlab <- ci$Variable
xlab <- factor(xlab, levels = xlab)
xlab <- fct_rev(xlab)

# plot confidence intervals
p1 <- ggplot() +
  geom_pointrange(ci, mapping = aes(x = xlab, y = mean, ymin = low, ymax = high, col = low < 0 & high > 0)) +
  scale_colour_manual(values = setNames(c("gray", "blue"), c(T, F))) +
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
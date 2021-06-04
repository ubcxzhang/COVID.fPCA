# scores heatmap

library(tidyverse)
library(choroplethr)

heat.map <- function(scores, title = "Heatmap", legend = "Value") {
  choro <- CountyChoropleth$new(scores)
  choro$title <- title
  choro$ggplot_scale <- scale_fill_brewer(name = legend, palette = "YlOrRd")
  choro$render()
}

fpca <- readRDS("./output/fpca.rds")

# save the first FPC score
PC1_score <- fpca$xiEst[, 1]
# saveRDS(PC1_score, "./output/PC1_score.rds")
PC2_score <- fpca$xiEst[, 2]
# saveRDS(PC2_score, "./output/PC2_score.rds")

# heatmap size: 4.5 * 10 landscape
heatmap_data <- data.frame(region = readRDS("./output/fips.rds"), value = PC1_score)
heat.map(heatmap_data, "The First Functional Principal Component Scores", "Scores")

raw <- read_csv("./data/1-31-21.csv")

data(df_pop_county)

covid <- raw[raw$FIPS %in% df_pop_county$region, ]

covid <- data.frame(County = covid[readRDS("./output/lc5.rds")$id.retain, 11], First_FPC_Score = PC1_score)
colnames(covid)[1] <- "County"
# write_csv(covid, "./output/PC1_score.csv")

top10 <- arrange(covid, desc(First_FPC_Score))[1:10, ]
top10[, 2] <- signif(top10[, 2], 5)
# write_csv(top10, "./output/top10.csv")

bot10 <- arrange(covid, First_FPC_Score)[1:10, ]
bot10[, 2] <- signif(bot10[, 2], 5)
# write_csv(bot10, "./output/bot10.csv")

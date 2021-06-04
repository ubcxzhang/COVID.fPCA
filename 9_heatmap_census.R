# heatmap

library(tidyverse)
library(choroplethr)

heat.map <- function(scores, title = "Heatmap", legend = "Value") {
  choro <- CountyChoropleth$new(scores)
  choro$title <- title
  choro$ggplot_scale <- scale_fill_brewer(name = legend, palette = "YlOrRd")
  choro$render()
}

var_names <- readRDS("./output/var_names.rds")

census <- readRDS("./output/combined_lockdown_census.rds")[, 1:15]

legends <- rep("Value", 14)

FIPS <- readRDS("./output/fips.rds")

for (i in 2:ncol(census)) {
  # heatmap size: 4.5 * 10 landscape
  jpeg(paste0("./figs/census/", i - 1, ".jpeg"), width = 10, height = 4.3, units = "in", res = 100)
  heatmap_data <- data.frame(region = FIPS, value = pull(census[, i]))
  print(heat.map(heatmap_data, var_names[i - 1], legends[i - 1]))
  dev.off()
}

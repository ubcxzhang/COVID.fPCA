# preprocess data

library(tidyverse)
library(choroplethr)

subdata <- function(logcounts,
                    minEvent = 5,
                    minDay = 5) {
  n.samples <- nrow(logcounts)
  n.day <- ncol(logcounts)

  starts <- rep(NA, nrow(logcounts))

  for (ii in 1:n.samples) {
    index_smaller <- which(logcounts[ii, ] < log(minEvent + 1))
    if (length(index_smaller) == 0) {
      starts[ii] <- 1
      next
    }
    if (index_smaller[length(index_smaller)] == n.day) next

    starts[ii] <- index_smaller[length(index_smaller)] + 1
  }

  Ly <- Lt <- vector("list", nrow(logcounts))

  for (ii in 1:length(Ly))
  {
    if (!is.na(starts[ii])) {
      if (starts[ii] == 1) {
        Ly[[ii]] <- logcounts[ii, ]
      } else {
        Ly[[ii]] <- logcounts[ii, -(1:(starts[ii] - 1))]
      }
      Lt[[ii]] <- 1:(n.day - starts[ii] + 1)
    }
  }

  id.retain <- (sapply(Ly, length) > 0)
  Ly <- Ly[id.retain]
  Lt <- Lt[id.retain]
  starts <- starts[id.retain]
  return(list(Ly = Ly, Lt = Lt, id.retain = id.retain, minEvent = minEvent, minDay = minDay, day1 = starts))
}

raw <- read_csv("./data/1-31-21.csv")

data(df_pop_county)

covid <- raw[raw$FIPS %in% df_pop_county$region, ]

logcounts <- log(as.matrix(covid[, -(1:11)]) + 1)
lc5 <- subdata(logcounts, minEvent = 5)
# saveRDS(lc5, "./output/lc5.rds")

fips <- unlist(covid[lc5$id.retain, "FIPS"], use.names = FALSE)
# saveRDS(fips, "./output/fips.rds")

# modified from fdapace package

DesignPlotCount <- function(t, obsGrid, noDiagonal, isColorPlot) {
  N <- length(obsGrid)
  res <- matrix(0, nrow = N, ncol = N)

  for (cur in t) {
    curidx <- match(cur, obsGrid)
    if (isColorPlot == FALSE) {
      res[curidx, curidx] <- 1
    } else {
      res[curidx, curidx] <- res[curidx, curidx] + 1
    }
  }

  if (noDiagonal == TRUE) {
    diag(res) <- 0
  }

  return(res)
}

CreateDesignPlot <- function(Lt, obsGrid = NULL, isColorPlot = TRUE, noDiagonal = TRUE, addLegend = TRUE, ...) {
  if (class(Lt) != "list") {
    stop("You do need to pass a list argument to 'CreateDesignPlot'!")
  }
  if (is.null(obsGrid)) {
    obsGrid <- sort(unique(unlist(Lt)))
  }

  args1 <- list(main = "Design Plot", xlab = "Observed time grid", ylab = "Observed time grid", addLegend = addLegend)
  inargs <- list(...)
  args1[names(inargs)] <- inargs

  if ((length(obsGrid) > 101) & all(sapply(Lt, function(u) identical(obsGrid, u)))) {
    res <- matrix(length(Lt), nrow = 101, ncol = 101)
    obsGrid <- approx(
      x = seq(0, 1, length.out = length(obsGrid)), y = obsGrid,
      xout = seq(0, 1, length.out = 101)
    )$y
  } else {
    res <- DesignPlotCount(Lt, obsGrid, noDiagonal, isColorPlot)
  }

  oldpty <- par()[["pty"]]
  on.exit(par(pty = oldpty))
  par(pty = "s")
  if (isColorPlot == TRUE) {
    createColorPlot(res, obsGrid, args1)
  } else {
    createBlackPlot(res, obsGrid, args1)
  }
}

createColorPlot <- function(res, obsGrid, args1) {
  res[res > 4] <- 4
  notZero <- which(res != 0, arr.ind = TRUE)
  nnres <- res[notZero]

  addLegend <- args1$addLegend
  args1$addLegend <- NULL

  if (is.null(args1$col)) {
    colVec <- c(`1` = "black", `2` = "blue", `3` = "green", `4` = "red")
    args1$col <- colVec[nnres]
  } else {
    colVec <- args1$col
  }

  if (is.null(args1$pch)) {
    pchVec <- rep(19, length(colVec))
    args1$pch <- pchVec[nnres]
  } else {
    pchVec <- args1$pch
  }

  if (is.null(args1$cex)) {
    cexVec <- seq(from = 0.3, by = 0.1, length.out = length(colVec))
    args1$cex <- cexVec[nnres]
  } else {
    cexVec <- args1$cex
  }

  do.call(plot, c(args1, list(x = obsGrid[notZero[, 1]], y = obsGrid[notZero[, 2]])))

  pars <- par()
  if (addLegend) {
    if (!identical(unique(nnres), 1)) {
      legend("right", c("1", "2", "3", "4+"),
        pch = pchVec, col = colVec, pt.cex = 0.5, title = "Count", bg = "white",
        inset = -pars[["mai"]][4] / pars[["pin"]][1] * 1.5, xpd = TRUE
      )
    }
  }
}

fpca <- readRDS("./output/fpca.rds")

par(mfrow = c(1, 2))

CreateScreePlot(fpca)

addLegend <- F
workGrid <- fpca$workGrid
fpcaObj <- fpca

k <- 1
if (addLegend) {
  newplt <- par()[["plt"]]
  newplt[2] <- newplt[1] + 0.85 * (newplt[2] - newplt[1])
  par(plt = newplt)
}
matplot(workGrid, fpcaObj$phi[, 1:k],
  type = "n",
  main = paste(collapse = "", c("First Eigenfunction")), xlab = "s", ylab = ""
)
abline(h = 0, col = "gray9")
grid()
matlines(workGrid, fpcaObj$phi[, 1:k])
pars <- par()
if (addLegend) {
  legend("right", col = 1:k, lty = 1:k, legend = do.call(expression, sapply(1:k, function(u) {
    return(bquote(phi[.(u)]))
  })), border = FALSE, xpd = TRUE, inset = -pars[["mai"]][4] / pars[["pin"]][1] * 1.8, seg.len = 1.2)
}

# save as pdf 4.5 * 9

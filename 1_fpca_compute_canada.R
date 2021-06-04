# fpca

library(fdapace)
set.seed(0)

lc5 <- readRDS("./lc5.rds")

t1 <- Sys.time()
fd5 <- FPCA(Ly = lc5$Ly, Lt = lc5$Lt, optns = list(dataType = "Sparse", verbose = TRUE))
t2 <- Sys.time()

# save results
# saveRDS(fd5, "./fpca.rds")
# save.image(file = "./fpca_workspace.RData")
print(t2 - t1)

print(sessionInfo())

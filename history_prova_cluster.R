## prova clusters
#install.packages("fpc")

library(fpc)

sim.xy <- function(n, mean, sd) cbind(rnorm(n, mean[1], sd[1]),
                                      rnorm(n, mean[2],sd[2]))
xy <- rbind(sim.xy(100, c(0,0), c(.2,.2)),
            sim.xy(100, c(2.5,0), c(.4,.2)),
            sim.xy(100, c(1.25,.5), c(.3,.2)))
library(fpc)

km.boot <- clusterboot(xy, B=20, bootmethod="boot",
                       clustermethod=kmeansCBI,
                       krange=3, seed=15555)
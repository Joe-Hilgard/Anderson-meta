# p-curve estimates?
source("Simonsohn-ESCI.R")

for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      
      set = dat[filter,]
      plotloss(set$t, set$df, dmin=-.5, dmax = 1)
      mtext(name)
    }
  }
}

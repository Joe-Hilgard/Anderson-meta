# p-curve estimates?
source("Simonsohn-ESCI.R")

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

dir.create("./pcurve_plotdump/")

for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      
      # conduct and export p-curve
      saveName = paste("./pcurve_plotdump/", paste(i,j,k, sep="_"),".png", sep="")
      set = dat[filter,]
      par(mfrow=c(2,1))
      windows()
      plotloss(set$t, set$df, dmin=-.5, dmax = 1)
      mtext(name)
      print(name)
      savePlot(filename=saveName, type="png")
      
      hist(set$p.twotail[set$p.twotail<.05], 
           main = "p-curve",
           xlab = "p-value (two-tailed)",
           breaks=seq(0, .05, by=.01))
      graphics.off()
      
      # conduct and export sensitivity analysis
      sensFrame = sensitive_pcurve(set$t, set$df, dmin = -.5, dmax = 1)
      write.table(sensFrame, 
                  file = paste("./pcurve_plotdump/", paste(i, j, k, sep="_"), ".txt", sep = ""),
                  sep = "\t",
                  row.names = F)
    }
  }
}

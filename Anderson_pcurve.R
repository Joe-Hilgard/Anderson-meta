# p-curve estimates?
# Next step: Generated bootstrapped 95% CIs. That will take a lot of cycles!
source("Simonsohn-ESCI.R")
library(dplyr)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

dir.create("./pcurve_plotdump/")

pcurveOutput = data.frame("Outcome" = NULL, "Setting" = NULL, "Best-practices" = NULL,
                          "dhat" = NULL, "rhat" = NULL)

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
      x = c("best-only", "full")[k]
      saveName = paste("./pcurve_plotdump/", paste(i,j,x, sep="_"),".png", sep="")
      set = dat[filter,]
      #par(mfrow=c(2,1))
      windows()
      pcurve = plotloss(set$t, set$df, dmin=-.5, dmax = 1)
      mtext(name)
      print(name)
      pcurveOutput = 
        pcurveOutput %>% 
        rbind(data.frame("Outcome" = i, "Setting" = j, "Best" = c("Best-only", "All")[k],
                         "k" = sum(filter), "n" = sum(set$Sample.size),
                         "dhat_pcurve" = pcurve[1], "rhat_pcurve" = pcurve[2]))
      savePlot(filename=saveName, type="png")
      
      
      hist(set$p.twotail[set$p.twotail<.05], 
           main = "p-curve",
           xlab = "p-value (two-tailed)",
           breaks=seq(0, .05, by=.01))
      x = c("best-only", "full")[k]
      saveName = paste("./pcurve_plotdump/", paste(i,j,x,"curve", sep="_"),".png", sep="")
      savePlot(filename=saveName, type="png")
      graphics.off()
      
      # conduct and export sensitivity analysis
      sensFrame = cbind(Study.name = as.character(set$Study.name), 
                        sensitive_pcurve(set$t, set$df, dmin = -.5, dmax = 1))
      IDFrame = dat[filter, c("Study.name", "Full.Reference", 
                              "Correlation", "Fisher.s.Z")]
      sensFrame = right_join(IDFrame,
                             sensFrame,
                             by = "Study.name")
      write.table(sensFrame, 
                  file = paste("./pcurve_plotdump/", paste(i, j, k, sep="_"), ".txt", sep = ""),
                  sep = "\t",
                  row.names = F)
    }
  }
}

write.table(pcurveOutput, file = "pcurve_results.txt", row.names=F, sep="\t")

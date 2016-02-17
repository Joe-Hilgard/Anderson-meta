# p-curve estimates?
# Next step: Generated bootstrapped 95% CIs. That will take a lot of cycles!
source("Simonsohn-ESCI.R")
library(dplyr)
library(ggplot2)

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
      # Export v-plot
      savePlot(filename=saveName, type="png")
      dev.off()
      
      # Plot curve of relative frequencies
      set = set %>% filter(set$p.twotail < .05)
      set$bin = ceiling(set$p.twotail*100)/100
      tmp = data.frame(Var1 = as.factor(seq(.01, .05, .01)))
      tmp = left_join(tmp, data.frame(table(set$bin)/sum(table(set$bin))))
      tmp$Freq[is.na(tmp$Freq)] = 0
      ggplot(tmp, aes(x = Var1, y = Freq)) +
        geom_point() +
        geom_line(aes(group=1)) +
        scale_y_continuous("Percentage of test results", limits = c(0, 1)) +
        scale_x_discrete(expression(italic(p)~-value)) +
        theme_bw() +
        annotate("text") +
        ggtitle(name)
      saveName = paste("./pcurve_plotdump/", paste(i,j,x,"curve", sep="_"),".png", sep="")
      ggsave(filename=saveName)
      
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

# export for p-curve web app
tmp = dat %>% 
  filter(Outcome == "AggAff",
         Setting == "Exp",
         Best. == "y")
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggAff",
         Setting == "Exp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggAff",
         Setting == "Nonexp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

# AggBeh
tmp = dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Exp",
         Best. == "y")
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard

tmp = dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Exp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Nonexp",
         Best. %in% c("y"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Nonexp",
         Best. %in% c("y"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Nonexp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

# AggCog
tmp = dat %>% 
  filter(Outcome == "AggCog",
         Setting == "Exp",
         Best. == "y")
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggCog",
         Setting == "Exp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggCog",
         Setting == "Nonexp",
         Best. %in% c("y"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "AggCog",
         Setting == "Nonexp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

# PhysArous
tmp = dat %>% 
  filter(Outcome == "PhysArous",
         Setting == "Exp",
         Best. == "y")
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()

tmp = dat %>% 
  filter(Outcome == "PhysArous",
         Setting == "Exp",
         Best. %in% c("y", "n"))
paste0("t(", tmp$df, ")=", tmp$t, "\n") %>% 
  writeClipboard()


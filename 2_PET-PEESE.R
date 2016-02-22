# PET-PEESE of the meta-analysis
# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

## Read in PET-PEESE functions
source("PETPEESE_functions.R")
# get dplyr package for distinct()
library(dplyr)

# let's just loop through this stuff.
# Would be nicer if I knew all these sub-categories...
# Where is there actually data?
table(dat$Setting, dat$Outcome, dat$Best.)
# and collapsing over best/not-best?
table(dat$Setting, dat$Outcome)
# and publication style?
pubTable = 
  dat %>%
  distinct(Full.Reference, Study) %>%
  select(Full.Reference, Study, Pub) %>%
  arrange(Full.Reference, Study)
#View(pubTable)
table(pubTable$Pub)
# Could go back later and look at effect of stat. significance on pub/unpub.

# make directories to hold PETPEESE output and diagnostic output
dir.create("./petpeese_plotdump") 
dir.create("./petpeese_plotdump/petInfluence")
dir.create("./petpeese_plotdump/peeseInfluence")

# Statistic value export ----
outputFrame = NULL

for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies 
      modelPET = PET(dat[filter,])
      modelPEESE = PEESE(dat[filter,])
      modelNaiveFE = rma(yi = Fisher.s.Z, sei=Std.Err, method="FE", data=dat[filter,])
      # Had to increase tolerance on threshold b/c aggbeh/exp/best wasn't converging.
      modelNaiveRE = rma(yi = Fisher.s.Z, sei=Std.Err, method="REML", data=dat[filter,],
                         control=list(threshold=5e-4))
      # Output data frame
      output = data.frame(
        # ID data
        "Outcome" = i,
        "Setting" = j,
        "Best" = ifelse(k==1, "Best-only", "All"),
        "naive-FE.r" = tanh(modelNaiveFE$b[1]),
        "naive-RE.r" = tanh(modelNaiveRE$b[1]),
        "RE.Q" = modelNaiveRE$QE,
        "RE.Q.p" = modelNaiveRE$QEp,
        "RE.I2" = modelNaiveRE$I2,
        "RE.I2.lb" = confint(modelNaiveRE)$random["I^2(%)", "ci.lb"],
        "RE.I2.ub" = confint(modelNaiveRE)$random["I^2(%)", "ci.ub"],
        # PET stats
        "PET.b0" = modelPET$b[1],
        "PET.b0.se" = modelPET$se[1],
        "PET.b0.p" = modelPET$pval[1],
        "PET.b1" = modelPET$b[2],
        "PET.b1.se" = modelPET$se[2],
        "PET.b1.p" = modelPET$pval[2],
          #I wonder if adding a PET or PEESE meta-reg removes the heterogeneity?
        "PET.Q" = modelPET$QE,
        "PET.Q.p" = modelPET$QEp,
        "PET.I2" = modelPET$I2,
        "PET.I2.lb" = confint(modelPET)$random["I^2(%)", "ci.lb"],
        "PET.I2.ub" = confint(modelPET)$random["I^2(%)", "ci.ub"],
            # PEESE stats
        "PEESE.b0" = modelPEESE$b[1],
        "PEESE.b0.se" = modelPEESE$se[1],
        "PEESE.b0.p" = modelPEESE$pval[1],
        "PEESE.b1" = modelPEESE$b[2],
        "PEESE.b1.se" = modelPEESE$se[2],
        "PEESE.b1.p" = modelPEESE$pval[2],
        "PEESE.Q" = modelPEESE$QE,
        "PEESE.Q.p" = modelPEESE$QEp,
        "PEESE.I2" = modelPEESE$I2,
        "PEESE.I2.lb" = confint(modelPEESE)$random["I^2(%)", "ci.lb"],
        "PEESE.I2.ub" = confint(modelPEESE)$random["I^2(%)", "ci.ub"]
      )
      outputFrame = rbind(outputFrame, output)
    }
  }
}
# Convert beta estimates to Pearson r and make CIs
outputFrame$PET.r = tanh(outputFrame$PET.b0)
outputFrame$PET.r.LL = tanh(outputFrame$PET.b0 - 1.96*outputFrame$PET.b0.se)
outputFrame$PET.r.UL = tanh(outputFrame$PET.b0 + 1.96*outputFrame$PET.b0.se)
outputFrame$PEESE.r = tanh(outputFrame$PEESE.b0)
outputFrame$PEESE.r.LL = tanh(outputFrame$PEESE.b0 - 1.96*outputFrame$PEESE.b0.se)
outputFrame$PEESE.r.UL = tanh(outputFrame$PEESE.b0 + 1.96*outputFrame$PEESE.b0.se)
#Round to 3 decimals
outputFrame[,sapply(outputFrame, is.numeric)] = round(outputFrame[,sapply(outputFrame, is.numeric)],3)
# TODO: fix tiny p-values
# outputFrame %>% 
#   select(ends_with("p")) %>% 
#   mutate_each( , )
# View(outputFrame)

# May want to shave off some of the less-useful columns
write.table(outputFrame, "PETPEESE_output.txt", row.names=F, sep="\t")

# Funnel plots ----
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      windows()
      x = c("best-only", "full")[k]
      saveName = paste("./petpeese_plotdump/", paste(i,j,x, sep="_"),".png", sep="")
      print(name)
      
      # Conduct and plot PET-PEESE
      dat %>%
        subset(filter) %>%
        funnelPETPEESE(plotName = name)
      
      # Export plot
      savePlot(filename=saveName, type="png")
      graphics.off()

    # Fetch influence diagnostics and export
      # PET influence
    windows()
    dat %>%
      subset(filter) %>%
      PET %>%
      influence %>%
      plot
    x = c("best-only", "full")[k]
    saveNamePetInf = paste("./petpeese_plotdump/petInfluence/", 
                              paste(i,j,x, sep="_"),".png", sep="")
    savePlot(filename = saveNamePetInf, type="png")
    graphics.off()
    
      # PEESE influence
    windows()
    dat %>%
      subset(filter) %>%
      PEESE %>%
      influence %>%
      plot
    x = c("best-only", "full")[k]
    saveNamePeeseInf = paste("./petpeese_plotdump/peeseInfluence/", 
                           paste(i,j,x, sep="_"),".png", sep="")
    savePlot(filename = saveNamePeeseInf, type="png")
    graphics.off()
    }
  }
}

# Sensitivity analysis ---------
# To check an influential observation:
dat %>% subset(row.names(.)== 172)
# or use grep()
dat[grep("U06PB", dat$Study.name),]
# Big nasty loop
dir.create("./sensitivity_analyses/")
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies 
      # Conduct sensitivity analysis & blow it out to .txt file
      name = paste(i, j, k, sep = "_") %>% 
        paste0("./sensitivity_analyses/meta_", ., ".txt") 
      sensitivityPETPEESE(dat[filter,]) %>% 
        write.table(file = name, row.names=F, sep="\t")
    }
  }
}

# Unpublished dissertations -------
table(dat$Pub, dat$Setting, dat$Outcome)
library(ggplot2)
dat = 
  dat %>%
  mutate("Diss" = ifelse(dat$Pub == "Dissertation (unpub)", "Diss", "Not Diss"))
# Brady (2006) is already included as a peer-reviewed journal article
dat$Diss[grep("Brady, S. (2006).", dat$Full.Reference, fixed=T)] = "Not Diss"
table(dat$Diss)

dat %>%
  filter(Setting == "Exp") %>%
  ggplot(aes(x=Fisher.s.Z, y=Std.Err, pch=Diss)) +
  geom_point(cex=4) +
  scale_shape_manual(values=c(16, 1)) +
  scale_y_reverse()

dat$pch = ifelse(dat$Diss == "Diss", 9, 1)
dissDat = dat %>% filter(Setting == "Exp") 
dissMod = rma(yi = Fisher.s.Z, sei = Std.Err, data = dissDat)
funnel(dissMod, pch=1)
points(x = dissDat$Fisher.s.Z, y = dissDat$Std.Err, pch = dissDat$pch)

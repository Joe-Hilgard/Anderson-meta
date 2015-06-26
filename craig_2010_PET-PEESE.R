# PET-PEESE of the meta-analysis

# My greatest concern right now is how to deal with multiple entries for a study, 
#   e.g. study appears as raw and as partial

# According to Carter & McCullough's citation of Stanley & Doucouliagos (2007)
  # Use PET to see if the estimated effect size != 0
  # If estimated effect size == 0 then use PET estimate
  # If PET says estimated effect != 0 then use PEESE estimate
# PET: b0 is estimated sig. of effect after bias
#    : b1 is estimated sig. of pub bias

# NOTES:
# Std.Err seems to refer to Fisher.s.Z's standard error, which is convenient.
# Not sure what some of these subscripts mean on setting:
# "Exp", "Nonexp", and "Long" are obvious, 
#   but what are "NonexpS", "LongP", and "LongPs"?
# Would be nice to make x-axis always bottom out at 0

# I think "S" stands for whether sex was applied as a covariate
  # and/or the sexes were analyzed separately
# I think "P" stands for "partial"
# Similarly "Long.Dup" varies:
  # L for Longitudinal
  # LS for Longitudinal separated by sex
  # LT1s for ???
  # LT1 for ???

# Read in the data
dat = read.delim("cleaned_craig.txt", stringsAsFactors=F)

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

# Let's make a table of stats:
# Let's make the model objects, then extract the parameter stats
outputFrame = data.frame(
  # ID data
  "Outcome.Group" = NULL,
  "Design" = NULL,
  "Outcome.Type" = NULL,
  # PET stats
  "PET.b0" = NULL,
  "PET.b0.se" = NULL,
  "PET.b0.p" = NULL,
  "PET.b1" = NULL,
  "PET.b1.se" = NULL,
  "PET.b1.p" = NULL,
  # PEESE stats
  "PEESE.b0" = NULL,
  "PEESE.b0.se" = NULL,
  "PEESE.b0.p" = NULL,
  "PEESE.b1" = NULL,
  "PEESE.b1.se" = NULL,
  "PEESE.b1.p" = NULL
)
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies 
      modelPET = PET(dat[filter,])
      modelPEESE = PEESE(dat[filter,])
      output = data.frame(
        # ID data
        "Outcome" = i,
        "Design" = j,
        "Best" = ifelse(k==1, "Best-only", "All"),
        # PET stats
        "PET.b0" = modelPET$b[1],
        "PET.b0.se" = modelPET$se[1],
        "PET.b0.p" = modelPET$pval[1],
        "PET.b1" = modelPET$b[2],
        "PET.b1.se" = modelPET$se[2],
        "PET.b1.p" = modelPET$pval[2],
        # PEESE stats
        "PEESE.b0" = modelPEESE$b[1],
        "PEESE.b0.se" = modelPEESE$se[1],
        "PEESE.b0.p" = modelPEESE$pval[1],
        "PEESE.b1" = modelPEESE$b[2],
        "PEESE.b1.se" = modelPEESE$se[2],
        "PEESE.b1.p" = modelPEESE$pval[2]
      )
      outputFrame = rbind(outputFrame, output)
    }
  }
}
# Convert beta estimates to Pearson r and make CIs
outputFrame$PET.r = atanh(outputFrame$PET.b0)
outputFrame$PET.r.LL = atanh(outputFrame$PET.b0 - 1.96*outputFrame$PET.b0.se)
outputFrame$PET.r.UL = atanh(outputFrame$PET.b0 + 1.96*outputFrame$PET.b0.se)
outputFrame$PEESE.r = atanh(outputFrame$PEESE.b0)
outputFrame$PEESE.r.LL = atanh(outputFrame$PEESE.b0 - 1.96*outputFrame$PEESE.b0.se)
outputFrame$PEESE.r.UL = atanh(outputFrame$PEESE.b0 + 1.96*outputFrame$PEESE.b0.se)
#Round to 3 decimals
outputFrame[,sapply(outputFrame, is.numeric)] = round(outputFrame[,sapply(outputFrame, is.numeric)],3)
# fix tiny p-values
outputFrame[outputFrame == 0] = "< .001"
# View(outputFrame)

# May want to shave off some of the less-useful columns
write.table(outputFrame, "PETPEESE_output.txt", row.names=F)

# Let's make funnel plots:
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
      saveName = paste("./petpeese_plotdump/", paste(i,j,k, sep="_"),".png", sep="")
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
    saveNamePetInf = paste("./petpeese_plotdump/petInfluence/", 
                              paste(i,j,k, sep="_"),".png", sep="")
    savePlot(filename = saveNamePetInf, type="png")
    graphics.off()
    
      # PEESE influence
    windows()
    dat %>%
      subset(filter) %>%
      PEESE %>%
      influence %>%
      plot
    saveNamePeeseInf = paste("./petpeese_plotdump/peeseInfluence/", 
                           paste(i,j,k, sep="_"),".png", sep="")
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
      # Conduct sensitivity analysis & blow it out to .dat file
      name = 
        paste(i, j, k, sep = "_") %>% 
        paste("./sensitivity_analyses/", ., ".txt", sep="") 
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


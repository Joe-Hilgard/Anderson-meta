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

## Read in Peters functions
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

# make directories to hold Peters output and diagnostic output
dir.create("./peters_plotdump") 
dir.create("./peters_plotdump/linearInfluence")
dir.create("./peters_plotdump/quadraticInfluence")

# Statistic value export ----
outputFrame = data.frame(
  # ID data
  "Outcome.Group" = NULL,
  "Design" = NULL,
  "Outcome.Type" = NULL,
  # linear stats
  "peters_linear.b0" = NULL,
  "peters_linear.b0.se" = NULL,
  "peters_linear.b0.p" = NULL,
  "peters_linear.b1" = NULL,
  "peters_linear.b1.se" = NULL,
  "peters_linear.b1.p" = NULL,
  # peters_quadratic stats
  "peters_quadratic.b0" = NULL,
  "peters_quadratic.b0.se" = NULL,
  "peters_quadratic.b0.p" = NULL,
  "peters_quadratic.b1" = NULL,
  "peters_quadratic.b1.se" = NULL,
  "peters_quadratic.b1.p" = NULL
)
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for meta-reg
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies 
      modelpeters_linear = peters_linear(dat[filter,])
      modelpeters_quadratic = peters_quadratic(dat[filter,])
      output = data.frame(
        # ID data
        "Outcome" = i,
        "Design" = j,
        "Best" = ifelse(k==1, "Best-only", "All"),
        # peters_linear stats
        "peters_linear.b0" = modelpeters_linear$b[1],
        "peters_linear.b0.se" = modelpeters_linear$se[1],
        "peters_linear.b0.p" = modelpeters_linear$pval[1],
        "peters_linear.b1" = modelpeters_linear$b[2],
        "peters_linear.b1.se" = modelpeters_linear$se[2],
        "peters_linear.b1.p" = modelpeters_linear$pval[2],
        # peters_quadratic stats
        "peters_quadratic.b0" = modelpeters_quadratic$b[1],
        "peters_quadratic.b0.se" = modelpeters_quadratic$se[1],
        "peters_quadratic.b0.p" = modelpeters_quadratic$pval[1],
        "peters_quadratic.b1" = modelpeters_quadratic$b[2],
        "peters_quadratic.b1.se" = modelpeters_quadratic$se[2],
        "peters_quadratic.b1.p" = modelpeters_quadratic$pval[2]
      )
      outputFrame = rbind(outputFrame, output)
    }
  }
}
# Convert beta estimates to Pearson r and make CIs
outputFrame$peters_linear.r = atanh(outputFrame$peters_linear.b0)
outputFrame$peters_linear.r.LL = atanh(outputFrame$peters_linear.b0 - 1.96*outputFrame$peters_linear.b0.se)
outputFrame$peters_linear.r.UL = atanh(outputFrame$peters_linear.b0 + 1.96*outputFrame$peters_linear.b0.se)
outputFrame$peters_quadratic.r = atanh(outputFrame$peters_quadratic.b0)
outputFrame$peters_quadratic.r.LL = atanh(outputFrame$peters_quadratic.b0 - 1.96*outputFrame$peters_quadratic.b0.se)
outputFrame$peters_quadratic.r.UL = atanh(outputFrame$peters_quadratic.b0 + 1.96*outputFrame$peters_quadratic.b0.se)
#Round to 3 decimals
outputFrame[,sapply(outputFrame, is.numeric)] = round(outputFrame[,sapply(outputFrame, is.numeric)],3)
# fix tiny p-values
outputFrame[outputFrame == 0] = "< .001"
# View(outputFrame)

# May want to shave off some of the less-useful columns
write.table(outputFrame, "peters.txt", row.names=F)

# Funnel plots ----
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for meta-reg
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 10) next # must have at least ten studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      windows()
      saveName = paste("./peters_plotdump/", paste(i,j,k, sep="_"),".png", sep="")
      print(name)
      
      # Conduct and plot meta-reg
      dat %>%
        subset(filter) %>%
        funnelPeters(plotName = name)
      
      # Export plot
      savePlot(filename=saveName, type="png")
      graphics.off()

    # Fetch influence diagnostics and export
      # peters_linear influence
    windows()
    dat %>%
      subset(filter) %>%
      peters_linear %>%
      influence %>%
      plot
    saveNamePetInf = paste("./peters_plotdump/linearInfluence/", 
                              paste(i,j,k, sep="_"),".png", sep="")
    savePlot(filename = saveNamePetInf, type="png")
    graphics.off()
    
      # peters_quadratic influence
    windows()
    dat %>%
      subset(filter) %>%
      peters_quadratic %>%
      influence %>%
      plot
    saveNamePeeseInf = paste("./peters_plotdump/quadraticInfluence/", 
                           paste(i,j,k, sep="_"),".png", sep="")
    savePlot(filename = saveNamePeeseInf, type="png")
    graphics.off()
    }
  }
}

# Sensitivity analysis (NOT READY YET) ---------
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


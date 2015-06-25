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

# To check an influential observation:
dat %>% subset(row.names(.)== 172)
# or use grep()
dat[grep("U06PB", dat$Study.name),]

# Aggressive Affect, Experiments -----------
# Removing Ballard & Wiest from AggAff.Exp
  # best-practices estimates
temp = 
  dat %>% 
  subset(Outcome == "AggAff" & 
           Setting == "Exp" & 
           Best. %in% "y") %>%
  subset(Study.name != "BW96AA")
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot

  # all-studies estimates
temp = 
  dat %>% 
  subset(Outcome == "AggAff" & 
           Setting == "Exp" & 
           Best. %in% c("y", "n")) %>%
  subset(Study.name != "BW96AA")
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot

# Aggressive Affect, Cross-sectional -------
# Removing Yukawa & Sakamoto from AggAff.Nonexp.Best
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") & 
           Study.name != "YS01AAb")
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot  

# Removing Urashima & Suzuki (2003) from AggAff.Nonexp.All
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("US03AA")) )
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot  
# Then removing Uozumi (2006)
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("US03AA", "U06AA")) )
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot 
# Then removing Matsuzaki et al (2004) study 2
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("US03AA", "U06AA", "MWS04AA2b")) )
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot 
# Then removing Yukawa & Sakamoto (2001)
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("US03AA", "U06AA", "MWS04AA2b", "YS01AAb")) )
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot 
# Then removing National Assembly for Youth Development (2006)
temp = 
  dat %>% 
  subset(Outcome=="AggAff" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("US03AA", "U06AA", "MWS04AA2b", 
                               "YS01AAb", "NA06AA")) )
funnelPETPEESE(temp)
temp %>% PET %>% influence %>% plot
temp %>% PEESE %>% influence %>% plot 

# Aggressive Behavior, Experiments -----
# Anderson Gentile & Buckley was previously influential when
  # we were using a multiplicative error term (e.g. lm() not rma())
  # It is no longer influential
# There were no outliers.

# Aggressive Behavior, Cross-sectional -----
# Best-practices
# Removing Youth Affairs Admin (1999) d = 1.45
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Anderson et al (2004) study 4 d = 0.71
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb", "ACF04AB4b")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Anderson et al (2004) study 4 d = 0.71
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb", "ACF04AB4b")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Ferguson et al. (2008) d = 3.76
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb", "ACF04AB4b", "FRC08AB1c")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Gentile & Gentile (2008) study 3, d = 3.3
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb", "ACF04AB4b", "FRC08AB1c",
                               "GG08AB3")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot

# Remvoing Carnagey & Anderson (2005) study 3, d = 0.58
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y") &
           !(Study.name %in% c("YA99ABb", "ACF04AB4b", "FRC08AB1c",
                               "GG08AB3", "CA05AB3c")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot

# All studies
# Removing Youth Affairs Administration (1999) study 1, d = 4.08
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Santisteban et al (2007) study 1, d = 2.8
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Gentile & Gentile (2008) study 3, d = 0.98
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Grusser et al. (2007) study 1, d = 0.96
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3",
                               "GTG87AB")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Anderson et al. (2004) study 4, d = 1.02
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3",
                               "GTG87AB", "ACF04AB4b")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Durkin & Barber (2002) study 1, d = 0.62
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3",
                               "GTG87AB", "ACF04AB4b", "DB02AB")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Endo et al (2007) study 1, d = 0.93
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3",
                               "GTG87AB", "ACF04AB4b", "DB02AB",
                               "EHY07AB")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Removing Wittman et al. (2008) study 2, d = 1.06
temp = 
  dat %>% 
  subset(Outcome=="AggBeh" & 
           Setting=="Nonexp" & 
           Best. %in% c("y", "n") &
           !(Study.name %in% c("YA99ABb", "SAR07AB", "GG08AB3",
                               "GTG87AB", "ACF04AB4b", "DB02AB",
                               "EHY07AB", "WAS08AB2")))
funnelPETPEESE(temp)
PET(temp) %>% influence %>% plot
PEESE(temp) %>% influence %>% plot
# Christ, there are still outliers. 
# Making me think that leave-one-out sensitivity analysis would be better

# Aggressive Cognition, Experiments ------
# No outliers for AggCog.Exp.Best
# Nor for AggCog.Exp.All

# Aggressive Cognition, Cross-sectional ----
# Removing from AggCog.Nonexp.Best?
# Then AB&G 2007 is outlier too
verbosePET(dat[dat$Outcome=="AggCog" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y") 
               & !(rownames(dat)%in%c(144, 351)),])
# Again having to remove Matsuzaki from AggCog.Nonexp.All
verbosePET(dat[dat$Outcome=="AggCog" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(144)),])
# What's that one out there? It's Santisteban et al 2007
leveragePET(dat[dat$Outcome=="AggCog" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(144)),]
            , id.n=10)
verbosePET(dat[dat$Outcome=="AggCog" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(144,218)),])

# ### Things got messy here as I played outlier whack-a-mole
# # Removing both Matsuzaki as outlier
# verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
#                & dat$Best. %in% c("y", "n") 
#                & !(rownames(dat)%in%c(187, 131)),])
# # & then Kuntsche too is outlier
# verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
#                & dat$Best. %in% c("y", "n") 
#                & !(rownames(dat)%in%c(187, 131,219)),])
# # & then there's another outlier
# leveragePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
#                & dat$Best. %in% c("y", "n") 
#                & !(rownames(dat)%in%c(187, 131,219)),]
#            , id.n = 10)
# # it's Rudatsikira, Muula, & Siziya (2008)
# verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
#                & dat$Best. %in% c("y", "n") 
#                & !(rownames(dat)%in%c(187, 131,219,472)),])

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


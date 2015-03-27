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

# let's just loop through this stuff.
# Would be nicer if I knew all these sub-categories...
# Where is there actually data?
table(dat$Setting, dat$Outcome, dat$Best.)
# and collapsing over best/not-best?
table(dat$Setting, dat$Outcome)

# make directories to hold PETPEESE output and diagnostic output
dir.create("./petpeese_plotdump") 
dir.create("./petpeese_plotdump/petInfluence")
dir.create("./petpeese_plotdump/peeseInfluence")

# Let's do this:
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
        funnelPETPEESE(naiveModel = naive(.), 
                       petModel = PET(.), 
                       peeseModel = PEESE(.), 
                       plotName = name)
      
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
# 
# # Can I get all the leverages together?
# for (i in unique(dat$Outcome)) {
#   for (j in c("Exp", "Nonexp")) {
#     for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
#       best = list("y", c("n", "y"))
#       filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
#       if (sum(filter) < 10) next # must have at least ten studies
#       name = paste("Outcome: ", i,
#                    ", Setting: ", j,
#                    ", Best?: ", k
#                    , sep="")
#       windows()
#       saveName = paste("./petpeese_plotdump/diagnostics/", paste(i,j,k, sep="_"),".png", sep="")
#       print(name)
#       par(mfrow=c(2,2))
#       leveragePET(dat[filter,], plotName = name)
#       savePlot(filename=saveName, type="png")
#       graphics.off()
#     }
#   }
# }

# To check an influential observation:
dat[row.names(dat)== 172, ]
# or use grep()
dat[grep("U06PB", dat$Study.name),]

# Removing Ballard & Wiest from AggAff.Exp
funnelPET.RMA(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y") & 
                 dat$Study.name != "BW96AA",])
leveragePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
                & dat$Best. %in% c("y") & 
                  dat$Study.name != "BW96AA",])
funnelPET.RMA(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y", "n") & 
                 dat$Study.name != "BW96AA",])
leveragePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y", "n") & 
                 dat$Study.name != "BW96AA",])
# Removing Yukawa & Sakamoto from AggAff.Nonexp.Best
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y") & rownames(dat)!=129,])
# Removing Uozumi and Urashima & Suzuki from AggAff.Nonexp.All
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c("179", "180")),])
# AggBeh.Exp.Best
leveragePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
                & dat$Best. %in% c("y"),], id.n=8)
# It's AG&B 07, study 1
funnelPET.RMA(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y") 
               & dat$Study.name != "AGB07AB1oe/AGB07AB1ye",])
leveragePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
                  & dat$Best. %in% c("y") 
                  & dat$Study.name != "AGB07AB1oe/AGB07AB1ye",])
# AggBeh.Exp.All, removing Panee & Ballard (2002) b/c it isn't agg behavior
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(402)),])
# AggBeh.Nonexp.Best, removing Matsuzaki
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y") 
               & !(rownames(dat)%in%c(131)),])
# AggBeh.Nonexp.All, using the one Matsuzaki & not other
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187)),])
# Removing both Matsuzaki as outlier
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187, 131)),])
# & then Kuntsche too is outlier
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187, 131,219)),])
# & then there's another outlier
leveragePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187, 131,219)),]
           , id.n = 10)
# it's Rudatsikira, Muula, & Siziya (2008)
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187, 131,219,472)),])
# No outliers for AggCog.Exp.Best
# Nor for AggCog.Exp.All
# Removing Matsuzaki from AggCog.Nonexp.Best
verbosePET(dat[dat$Outcome=="AggCog" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y") 
               & !(rownames(dat)%in%c(144)),])
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
## Influence Plot 
#influencePlot(fit,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

## PEESE
# AggBeh.Exp.Best
verbosePEESE(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
                 & dat$Best. %in% c("y", "n"),])
leveragePEESE(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
                 & dat$Best. %in% c("y", "n"),])
# and minus outliers from PET
verbosePEESE(dat[dat$Outcome=="AggBeh" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c(187, 131,219,472)),])

# i = "AggAff"; j = "Exp"; k = "y"
# filter = dat$Outcome == i & dat$Setting == j & dat$Best. == k
# res =  rma(Correlation, Std.Err^2, data=dat[filter,], measure="COR")
# funnel(dat$Correlation[filter], dat$Std.Err[filter], xlim=c(0,1), contour=c(.9,.95,.975))

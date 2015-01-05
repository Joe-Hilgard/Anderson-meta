# PET-PEESE of the meta-analysis
# Now I intend to add package 'metafor' and use rma()
  # to reproduce Craig's estimates alongside the PET-PEESE

# My greatest concern right now is how to deal with multiple entries for a study, 
#   e.g. study appears as raw and as partial, or for men and for women

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

require(metafor)
require(meta)

# Read in the data
setwd("C:/Users/bartholowlab/Documents/GitHub/Craig_meta")
dat=read.delim("Craig_Table_2010.txt", stringsAsFactors=F)
dat = dat[dat$Best.!="",] # delete the blank row
# Std.Err refers to Std.Err of z-transformed value

# dump the use of sex as a control for now in dataset  # Later to be made separate dat1
dat = dat[dat$Setting %in% c("Exp", "Nonexp", "Long") 
          # in case this is the only way the correlational study was reported:
          | (dat$Setting %in% "NonexpS" & dat$SEX %in% c("M", "F"))  
          # or for longitudinal studies
          #| (dat$Setting %in% "LongPs") 
          ,]
dat$Setting[dat$Setting == "NonexpS"] = "Nonexp"
#dat$Setting[dat$Setting %in% c("LongP", "LongPs"]

## Create functions
# PET
PET=function(dataset) {
  petOut = lm(Fisher.s.Z ~ Std.Err, weights=1/(Std.Err^2), data=dataset)
  return(petOut)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
}
verbosePET=function(dataset, plotName=NULL) {
  petOut = lm(Fisher.s.Z ~ Std.Err, weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
  with(dataset, plot(x=Std.Err, y=Fisher.s.Z, main=plotName
                     , xlim=c(0, max(Std.Err))
                     , ylim=c(min(petOut$coefficients[1], Fisher.s.Z, na.rm=T), max(petOut$coefficients[1], Fisher.s.Z, na.rm=T))
                     )
       )
  abline(petOut)
  abline(h=petOut$coefficients[1], col='blue'); abline(v=0); abline(h=0)
  mtext(paste("r = ", round(atanh(petOut$coefficients[1]), 2)
              , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dat[filter,]
                    , measure="COR", method="FE")$b[1]), 2))
        , side=1)
}
leveragePET = function(dataset, plotName=NULL, id.n=3) {
  petOut = lm(Fisher.s.Z ~ Std.Err, weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
  plot(petOut#, labels.id=dataset$Study.name
       , id.n=id.n
       )
}
funnelPET = function(dataset, ...) {
  funnel(dataset$Fisher.s.Z, dataset$Std.Err, ...)
  petOut = PET(dataset)
  abline(a = -petOut$coefficients[1]/petOut$coefficients[2]
         , b = 1/petOut$coefficients[2])
  mtext(paste("r = ", round(atanh(petOut$coefficients[1]), 2)
              , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dat[filter,]
                                , measure="COR", method="FE")$b[1]), 2))
        , side=1)
  points(x = atanh(petOut$coefficients[1]), y=0, cex=1.5)
}
funnelPET.RMA = function(dataset, ...) {
  funnel(rma(Fisher.s.Z, Std.Err^2, data=dataset, measure="COR",...))
  petOut = PET(dataset)
  abline(a = -petOut$coefficients[1]/petOut$coefficients[2]
         , b = 1/petOut$coefficients[2])
  mtext(paste("r = ", round(atanh(petOut$coefficients[1]), 2)
              , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dat[filter,]
                                , measure="COR", method="FE")$b[1]), 2))
        , side=1)
  points(x = atanh(petOut$coefficients[1]), y=0, cex=1.5, pch=7)
}
# PEESE
PEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err^2, weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  return(peeseOut)
}
verbosePEESE=function(dataset, plotName=NULL) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  with(dataset, plot(x=Std.Err^2, y=Fisher.s.Z, main=plotName
                     , xlim=c(0, max(Std.Err^2))
                     , ylim=c(min(peeseOut$coefficients[1], Fisher.s.Z, na.rm=T), max(peeseOut$coefficients[1], Fisher.s.Z, na.rm=T))
  )
  )
  abline(peeseOut)
  abline(h=peeseOut$coefficients[1], col='blue'); abline(v=0); abline(h=0)
  mtext(paste("r = ", round(atanh(peeseOut$coefficients[1]), 2)
              , ", p-effect = ", round(summary(peeseOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(peeseOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dataset
                                , measure="COR", method="FE")$b[1]), 2))
        , side=1)
}
leveragePEESE = function(dataset, plotName=NULL, id.n=3) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  plot(peeseOut#, labels.id=dataset$Study.name
       , id.n=id.n
  )
}

# let's just loop through this stuff.
# Would be nicer if I knew all these sub-categories...
# Where is there actually data?
table(dat$Setting, dat$Outcome, dat$Best.)
# and collapsing over best/not-best?
table(dat$Setting, dat$Outcome)

# make directories to hold PETPEESE output and diagnostic output
dir.create("./petpeese_plotdump"); dir.create("./petpeese_plotdump/diagnostics")

# Let's do this:
for (i in unique(dat$Outcome)) {
  for (j in unique(dat$Setting)) {
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      windows()
      saveName = paste("./petpeese_plotdump/", paste(i,j,k, sep="_"),".png", sep="")
      print(name)
      verbosePET(dat[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}

# Can I get all the leverages together?
for (i in unique(dat$Outcome)) {
  for (j in unique(dat$Setting)) {
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      windows()
      saveName = paste("./petpeese_plotdump/diagnostics/", paste(i,j,k, sep="_"),".png", sep="")
      print(name)
      par(mfrow=c(2,2))
      leveragePET(dat[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}

# To check an influential observation:
dat[row.names(dat)== 235, ]
# or use grep()
dat[grep("U06PB", dat$Study.name),]

# Removing Ballard & Wiest from AggAff.Exp
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y") & rownames(dat)!=235,])
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y", "n") & rownames(dat)!=235,])
# Removing Yukawa & Sakamoto from AggAff.Nonexp.Best
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y") & rownames(dat)!=129,])
# Removing Uozumi and Urashima & Suzuki from AggAff.Nonexp.All
verbosePET(dat[dat$Outcome=="AggAff" & dat$Setting=="Nonexp" 
               & dat$Best. %in% c("y", "n") 
               & !(rownames(dat)%in%c("179", "180")),])
# AggBeh.Exp.Best. IDing and removing AG&B 2007 from book
dat$Full.Reference[rownames(dat) %in% c(251, 253, 254)]
plot(lm(Fisher.s.Z ~ Std.Err, weights=(1/Std.Err^2), 
        data=dat[dat$Outcome=="AggBeh"
                 & dat$Setting=="Exp" & dat$Best. %in% c("y"),]), id.n=15)
dat$Full.Reference[rownames(dat)==245] # cook's d ~= .5 on AG&B 2007
verbosePET(dat[dat$Outcome=="AggBeh" & dat$Setting=="Exp" 
               & dat$Best. %in% c("y") 
               & !(rownames(dat)%in%c(245)),])
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

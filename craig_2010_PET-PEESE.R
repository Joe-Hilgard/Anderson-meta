# PET-PEESE of the meta-analysis
# Now I intend to add package 'metafor' and use rma()
  # to reproduce Craig's estimates alongside the PET-PEESE

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

# Read in the data
setwd("G:/Craig_meta")
dat=read.delim("Craig_Table_2010.txt", stringsAsFactors=F)
dat = dat[dat$Best.!="",] # delete the blank row
# Std.Err refers to Std.Err of z-transformed value

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
  mtext(paste("r = ", round(petOut$coefficients[1], 2)
              , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(rma(Fisher.s.Z, Std.Err^2, data=dat[filter,]
                    , measure="COR")$b[1], 2)
        , side=1)
}
# PEESE
PEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err^2, weights=1/(Std.Err^2), data=dataset)
  return(peeseOut)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
}
verbosePEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err^2, weights=1/(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  with(dataset, plot(x=Std.Err, y=Fisher.s.Z))
  abline(peeseOut)
}

# so for best-practices AggBeh?
filtBestAggBeh = dat$Long.Dup == "Exp" & dat$Outcome == "AggBeh"
petBestAggBeh = PET(dat[filtBestAggBeh,])
verbosePET(dat[filtBestAggBeh,])
verbosePEESE(dat[filtBestAggBeh,])

# and not-best-practices AggBeh?
filtNBAggBeh = dat$Long.Dup == "NB" & dat$Outcome == "AggBeh"
petNBAggBeh = PET(dat[filtNBAggBeh,])
verbosePET(dat[filtNBAggBeh,])
verbosePEESE(dat[filtNBAggBeh,])

# let's just loop through this stuff.
# Would be nicer if I knew all these sub-categories...
# Where is there actually data?
table(dat$Setting, dat$Outcome, dat$Best.)
## WARNING! I'm gonna coerce those weird Setting entries
  # to conform with the less-weird ones:
dat$Setting[dat$Setting %in% c("NonexpS")] = "Nonexp" 
dat$Setting[dat$Setting %in% c("LongPs", "LongP")] = "Long"
table(dat$Setting, dat$Outcome, dat$Best.)
# Let's do this:
for (i in unique(dat$Outcome)) {
  for (j in unique(dat$Setting)) {
    for (k in unique(dat$Best.)) {
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. == k
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

i = "AggAff"; j = "Exp"; k = "y"
filter = dat$Outcome == i & dat$Setting == j & dat$Best. == k
res =  rma(Correlation, Std.Err^2, data=dat[filter,], measure="COR")
funnel(dat$Correlation[filter], dat$Std.Err[filter], xlim=c(0,1), contour=c(.9,.95,.975))

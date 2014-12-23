# PET-PEESE of the meta-analysis
# Now I intend to add package 'metafor' and use rma()
  # to reproduce G&M's estimates alongside the PET-PEESE

# According to Carter & McCullough's citation of Stanley & Doucouliagos (2007)
  # Use PET to see if the estimated effect size != 0
  # If estimated effect size == 0 then use PET estimate
  # If PET says estimated effect != 0 then use PEESE estimate
# PET: b0 is estimated sig. of effect after bias
#    : b1 is estimated sig. of pub bias

# NOTES:
# Std.Err.1 seems to refer to Fisher.s.Z's standard error, which is convenient.
# Not sure what some of these subscripts mean on setting:
# "Exp", "Nonexp", and "Long" are obvious, 
#   but what are "NonexpS", "LongP", and "LongPs"?
# Would be nice to make x-axis always bottom out at 0

# Seems I've only got the violent-media stuff so I can't reproduce the
  # prosocial-media stuff. Could always ask...

require(metafor)

# Read in the data
setwd("G:/Craig_meta")
dat=read.delim("GM2014_full_raw_data.txt", stringsAsFactors=F)
table(dat$outcome.type, dat$Outcome.Group)

# for (i in c(2:6,11:14)) {
#   print (names(dat)[i])
#   print (unique(dat[,i]))
# }

# Need to determine how G&M sliced up these studies as reported in Tables 1 and 2...
# It's such a damn chore that several lines are reported for a single study like
#   the stupid 15min 10min 45min 30min stuff, I don't trust that.
# I could maybe start by just filtering by Study Design
#   And then maybe aggregate (weighted mean) within studies with same name & outcome 
#     but different other bullass.

# Std.Err.1 refers to Std.Err of z-transformed value

## Create functions # NOTE THAT THIS TIME WE USE Std.Err.1
# PET
PET=function(dataset) {
  petOut = lm(Fisher.s.Z ~ Std.Err.1, weights=1/(Std.Err.1^2), data=dataset)
  return(petOut)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
}
verbosePET=function(dataset, plotName=NULL) {
  petOut = lm(Fisher.s.Z ~ Std.Err.1, weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
  with(dataset, plot(x=Std.Err.1, y=Fisher.s.Z, main=plotName
                     , xlim=c(0, max(Std.Err.1))
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
              , round(atanh(rma(Fisher.s.Z, Std.Err.1^2, data=dat[filter,]
                                , measure="COR")$b[1]), 2))
        , side=1)
}
# PEESE
PEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err.1^2, weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  return(peeseOut)
}
verbosePEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err.1^2, weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  with(dataset, plot(x=Std.Err.1, y=Fisher.s.Z))
  abline(peeseOut)
}

# Let's do this shit
for (i in unique(dat$Outcome.Group)) {
  for (j in unique(dat$study.design)) {
    for (k in unique(dat$outcome.type)) {
      #print(paste(i,j,k))
      if (i == "" | j  == "" | k == "") next # skip stupid "" returns
      filter = dat$Outcome.Group == i & dat$study.design == j & dat$outcome.type == k
      if (sum(filter) < 4) next # must have at least four studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Type: ", k
                   , sep="")
      windows()
      saveName = paste("./GM_petpeese_plotdump/", paste(i,j,substr(k,1,4), sep="_"),".png", sep="")
      print(name)
      verbosePET(dat[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}

i = "AggAff"; j = "Exp"; k = "y"
filter = dat$Outcome == i & dat$Setting == j & dat$Best. == k
res =  rma(Correlation, Std.Err.1^2, data=dat[filter,], measure="COR")
funnel(dat$Correlation[filter], dat$Std.Err.1[filter], xlim=c(0,1), contour=c(.9,.95,.975))

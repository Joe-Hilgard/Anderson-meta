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
require(reshape2)


# Need to determine how G&M sliced up these studies as reported in Tables 1 and 2...
# It's such a damn chore that several lines are reported for a single study
# I could maybe start by just filtering by Study Design
#   And then maybe aggregate (weighted mean) within studies with same name & outcome 
# I'd like to double-check all the Krcmar stuff with the millions of simple slopes

# I need to reproduce CMA's imputation/aggregation/filtering
  # For subgroups within studies, sum N and weighted average effect size
  # For outcomes within studies, weighted average N and effect size 

# average across outcomes and comparisons within studies (is this ok?)
ids = names(dat)[!(names(dat) %in% c("Correlation", "Std.Err", "Fisher.s.Z", "Std.Err.1","N"))]
dat_m = melt(dat, id.vars=ids)
dat1 = dcast(dat_m, 
             Study + Subgroup.within.study + Outcome.Group +
               outcome.type + media.type + study.design + StudyShort~ variable,
             fun.aggregate = mean # would weighted.mean be possible?
             )
table(dat1$StudyShort)
# Then how do we handle subgroups?
# Std.Err.1 refers to Std.Err of z-transformed value

##View(dat1[dat1$study.design == "experimental" & dat1$Outcome.Group=="behavior",])  
# judging from this it looks like "Subgroup within study" should be aggregated across  
# Need to ask Dirk re: average, sum, or filter on that column.
# Anderson & Carnagey S3 sample N all screwed up b/c of this

# Still waiting on Dirk to advise re: summing vs averaging
ids1 = names(dat1)[!(names(dat1) %in% c("Correlation", "Std.Err", "Fisher.s.Z", "Std.Err.1","N"))]
dat1_m = melt(dat1, id.vars=ids1)
dat2 = dcast(dat1_m,
             Study + StudyShort + Outcome.Group + outcome.type +
               media.type + study.design ~ variable, fun.aggregate=mean)

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
                                , measure="COR", method="FE")$b[1]), 2))
        , side=1)
}
leveragePET = function(dataset, plotName=NULL, id.n=3) {
  petOut = lm(Fisher.s.Z ~ Std.Err.1, weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
  plot(petOut#, labels.id=dataset$Study.name
       , id.n=id.n
  )
}
# PEESE
PEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ Std.Err.1^2, weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  return(peeseOut)
}
verbosePEESE=function(dataset, plotName=NULL) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err.1^2), weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  with(dataset, plot(x=Std.Err.1^2, y=Fisher.s.Z, main=plotName
                     , xlim=c(0, max(Std.Err.1^2))
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
              , round(atanh(rma(Fisher.s.Z, Std.Err.1^2, data=dataset
                                , measure="COR", method="FE")$b[1]), 2))
        , side=1)
}
leveragePEESE = function(dataset, plotName=NULL, id.n=3) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err.1^2), weights=1/(Std.Err.1^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  plot(peeseOut#, labels.id=dataset$Study.name
       , id.n=id.n
  )
}

# Look for studies featuring a particular author
# View(dat2[grep("Anderson", dat$Study),])

# Make storage directories for output:
dir.create("./GM_petpeese_plotdump"); dir.create("./GM_petpeese_plotdump/diagnostics")

# Let's do this shit
for (i in unique(dat2$Outcome.Group)) {
  for (j in unique(dat2$study.design)) {
    for (k in unique(dat2$outcome.type)) {
      #print(paste(i,j,k))
      if (i == "" | j  == "" | k == "") next # skip stupid "" returns
      filter = dat2$Outcome.Group == i & dat2$study.design == j & dat2$outcome.type == k
      if (sum(filter) < 4) next # must have at least four studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Type: ", k
                   , sep="")
      windows()
      saveName = paste("./GM_petpeese_plotdump/", paste(i,j,substr(k,1,4), sep="_"),".png", sep="")
      print(name)
      verbosePET(dat2[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}
# Let's get influence diagnostics
for (i in unique(dat2$Outcome.Group)) {
  for (j in unique(dat2$study.design)) {
    for (k in unique(dat2$outcome.type)) {
      #print(paste(i,j,k))
      if (i == "" | j  == "" | k == "") next # skip stupid "" returns
      filter = dat2$Outcome.Group == i & dat2$study.design == j & dat2$outcome.type == k
      if (sum(filter) < 4) next # must have at least four studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Type: ", k
                   , sep="")
      windows()
      saveName = paste("./GM_petpeese_plotdump/diagnostics/", paste(i,j,substr(k,1,4), sep="_"),".png", sep="")
      print(name)
      par(mfrow=c(2,2))
      leveragePET(dat2[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}

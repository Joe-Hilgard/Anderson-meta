require(metafor)

# PET
PET=function(dataset) {
  petOut = lm(Fisher.s.Z ~ Std.Err, weights=1/I(Std.Err^2), data=dataset)
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
       , id.n=id.n, main=plotName
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
funnelPET.RMA = function(dataset, plotName=NULL) {
  funnel(rma(Fisher.s.Z, Std.Err^2, data=dataset, measure="GEN", method="FE"), main="")
  title(main=plotName, line=3)
  petOut = PET(dataset)
  abline(a = -petOut$coefficients[1]/petOut$coefficients[2]
         , b = 1/petOut$coefficients[2])
  mtext(paste("r = ", round(atanh(petOut$coefficients[1]), 2)
              , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
              , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
              , sep=""))
  mtext(paste("Naive meta estimate, r ="
              , round(atanh(rma(Fisher.s.Z, I(Std.Err^2), data=dataset
                                , measure="GEN", method="FE")$b[1]), 2))
        , side=1)
  points(x = (petOut$coefficients[1]), y=0, cex=1.5, pch=7)
  # plot conditional PEESE estimator
  if(summary(petOut)$coefficients[1,4] < .05) {
    peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/I(Std.Err^2), data=dataset)
    grid = data.frame(Std.Err=seq(0,.41,.001)^2)
    grid$Fisher.s.Z = predict(peeseOut, grid)
    lines(x=grid$Fisher.s.Z, y=grid$Std.Err, typ='l')
    points(x = (peeseOut$coefficients[1]), y=0, cex=1.5, pch=5)
    mtext(paste("PEESE r = ", round(atanh(peeseOut$coefficients[1]), 2)), line=1) 
  }
}
# PEESE
PEESE=function(dataset) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/I(Std.Err^2), data=dataset)
  print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
  return(peeseOut)
}
verbosePEESE=function(dataset, plotName=NULL) {
  peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/I(Std.Err^2), data=dataset)
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

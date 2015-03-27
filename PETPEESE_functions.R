require(metafor)
require(magrittr)

# naive meta-analysis
naive = function(dataset) {
  rma(yi = Fisher.s.Z,
      sei = Std.Err,
      data = dataset,
      method = "FE")
}

# PET
PET=function(dataset) {
  petOut = rma(yi = Fisher.s.Z, 
               sei = Std.Err, 
               mods = ~Std.Err, 
               data=dataset)
  return(petOut)
}

# PEESE
PEESE=function(dataset) {
  peeseOut = rma(yi = Fisher.s.Z, 
                 sei = Std.Err, 
                 mods = ~I(Std.Err^2), 
                 data=dataset)
  return(peeseOut)
}

# make a funnel plot with PET line and conditional PEESE line
funnelPETPEESE = function(naiveModel, petModel, peeseModel, 
                          alwaysPEESE=F, plotName=NULL, ...) {
  # make funnel plot
  funnel(naiveModel)
  title(plotName, line=3)
  naiveModel$b[1] %>% 
    atanh %>% 
    round(3) %>%
    paste("Naive meta estimate, r =", .) %>%
    mtext(side=1)
  # add line and text from PET
  petModel %$% 
    abline(a = -b[1]/b[2], b = 1/b[2])
  r = petModel$b[1] %>% atanh %>% round(3)
  p.effect = petModel$pval[1] %>% round(3)
  p.bias = petModel$pval[2] %>% round(3)
  mtext(paste("PET r = ", r
              , ", p-effect = ", p.effect
              , ", p-bias = ", p.bias
              , sep=""))
  points(x = petModel$b[1], y=0, cex=1.5)
  # add line and text from PEESE
# Debug stuff: Getting weird length for alwaysPEESE == T,
  # harmless bug, but annoying.
#   print(length(petModel$pval[1]))
#   print(length(alwaysPEESE))
#   print(length(petModel$pval[1] < .05 | alwaysPEESE == T))
  if(petModel$pval[1] < .05 | alwaysPEESE == T) {
    grid = 
      naiveModel$vi %>%
      raise_to_power(.5) %>%
      max %>%
      seq(0, ., .001) %>%
      data.frame("Std.Err" = .)
    grid$Var = grid$Std.Err^2
    grid$Fisher.s.Z = 
      peeseModel$b[1] + peeseModel$b[2]*grid$Var
    grid %$% lines(x=Fisher.s.Z, y=Std.Err, typ='l')
    points(x = (peeseModel$b[1]), y=0, cex=1.5, pch=5)
    peeseModel$b[1] %>%
      atanh %>%
      round(3) %>%
      paste("PEESE r =", .) %>%
      mtext(line = 1)
  }
}


# 
# funnelPEESE = function(dataset, plotName=NULL) {
#   peeseOut = PEESE(dataset)
#   naive = rma(yi=Fisher.s.Z, sei=Std.Err, data=dataset, method="FE")
#   # make funnel plot
#   funnel(naive)
#   title(plotName, line=3)
#   # get predicted values for PEESE curve
#   grid = 
#     seq(0, .41, .001) %>%
#     data.frame("Std.Err" = .)
#   grid$Var = grid$Std.Err^2
#   grid$Fisher.s.Z = 
#     peeseOut$b[1] + peeseOut$b[2]*grid$Var
#   grid %$% lines(x=Fisher.s.Z, y=Std.Err, typ='l')
#   points(x = (peeseOut$b[1]), y=0, cex=1.5, pch=5)
# }
# 
# # funnel plot with PET and conditional PEESE estimators
# funnelPET.RMA = function(dataset, plotName=NULL) {
#   petOut = PET(dataset)
#   funnel(rma(Fisher.s.Z, Std.Err^2, data=dataset, measure="GEN", method="FE"), main="")
#   title(main=plotName, line=3)
#   petOut = PET(dataset)
#   abline(a = -petOut$coefficients[1]/petOut$coefficients[2]
#          , b = 1/petOut$coefficients[2])
#   mtext(paste("r = ", round(atanh(petOut$coefficients[1]), 2)
#               , ", p-effect = ", round(summary(petOut)$coefficients[1,4], 3)
#               , ", p-bias = ", round(summary(petOut)$coefficients[2,4], 3)
#               , sep=""))
#   mtext(paste("Naive meta estimate, r ="
#               , round(atanh(rma(Fisher.s.Z, I(Std.Err^2), data=dataset
#                                 , measure="GEN", method="FE")$b[1]), 2))
#         , side=1)
#   points(x = (petOut$coefficients[1]), y=0, cex=1.5, pch=7)
#   # plot conditional PEESE estimator
#   if(summary(petOut)$coefficients[1,4] < .05) {
#     peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/I(Std.Err^2), data=dataset)
#     grid = data.frame(Std.Err=seq(0,.41,.001)^2)
#     grid$Fisher.s.Z = predict(peeseOut, grid)
#     lines(x=grid$Fisher.s.Z, y=grid$Std.Err, typ='l')
#     points(x = (peeseOut$coefficients[1]), y=0, cex=1.5, pch=5)
#     mtext(paste("PEESE r = ", round(atanh(peeseOut$coefficients[1]), 2)), line=1) 
#   }
# }
# I broke this but it was hideous anyway
# verbosePET=function(dataset, plotName=NULL) {
#   petOut = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err, data=dataset)
#   print(paste("Estimated effect size: r =", atanh(petOut$b[1])))
#   with(dataset, plot(x=Std.Err, y=Fisher.s.Z, main=plotName
#                      , xlim=c(0, max(Std.Err))
#                      , ylim=c(min(petOut$coefficients[1], Fisher.s.Z, na.rm=T), max(petOut$coefficients[1], Fisher.s.Z, na.rm=T))
#   )
#   )
#   abline(petOut)
#   abline(h=petOut$coefficients[1], col='blue'); abline(v=0); abline(h=0)
#   mtext(paste("r = ", round(atanh(petOut$b[1]), 2)
#               , ", p-effect = ", round(summary(petOut)$b[1,4], 3)
#               , ", p-bias = ", round(summary(petOut)$b[2,4], 3)
#               , sep=""))
#   mtext(paste("Naive meta estimate, r ="
#               , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dat[filter,]
#                                 , measure="COR", method="FE")$b[1]), 2))
#         , side=1)
# }
# also scrapping verbosePEESE()
# verbosePEESE=function(dataset, plotName=NULL) {
#   peeseOut = PEESE(dataset)
#   print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
#   with(dataset, plot(x=Std.Err^2, y=Fisher.s.Z, main=plotName
#                      , xlim=c(0, max(Std.Err^2))
#                      , ylim=c(min(peeseOut$coefficients[1], Fisher.s.Z, na.rm=T), max(peeseOut$coefficients[1], Fisher.s.Z, na.rm=T))
#   )
#   )
#   abline(peeseOut)
#   abline(h=peeseOut$coefficients[1], col='blue'); abline(v=0); abline(h=0)
#   mtext(paste("r = ", round(atanh(peeseOut$coefficients[1]), 2)
#               , ", p-effect = ", round(summary(peeseOut)$coefficients[1,4], 3)
#               , ", p-bias = ", round(summary(peeseOut)$coefficients[2,4], 3)
#               , sep=""))
#   mtext(paste("Naive meta estimate, r ="
#               , round(atanh(rma(Fisher.s.Z, Std.Err^2, data=dataset
#                                 , measure="COR", method="FE")$b[1]), 2))
#         , side=1)
# }

# don't need leveragePET() anymore, use metafor's plot(influence(<rma_object>))
# leveragePET = function(dataset, plotName=NULL, id.n=3) {
#   petOut = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err, data=dataset)
#   #print(paste("Estimated effect size: r =", atanh(petOut$coefficients[1])))
#   plot(petOut#, labels.id=dataset$Study.name
#        , id.n=id.n, main=plotName
#   )
# }
# leveragePEESE = function(dataset, plotName=NULL, id.n=3) {
#   peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), weights=1/(Std.Err^2), data=dataset)
#   print(paste("Estimated effect size: r =", atanh(peeseOut$coefficients[1])))
#   plot(peeseOut#, labels.id=dataset$Study.name
#        , id.n=id.n
#   )
# }

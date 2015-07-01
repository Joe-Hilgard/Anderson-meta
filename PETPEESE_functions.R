# install.packages(c('metafor', 'magrittr'))
library(metafor)
library(magrittr)

# I'm terrible with functions and global/local environment so
  # these functions simply assume your columns have names like
  # Comprehensive Meta-Analysis v3 gives them, e.g.
  # yi is "Fisher.s.Z",
  # sei is "Std.Err"
# If this assumption is wrong, please rename your columns
#   or change this code.
# If you're working in d instead of r, remove the tanh steps
# Also note that everything is in fixed-effects, not random, models

# Of these functions, the most useful is probably funnelPETPEESE()
# This compares the naive meta-analysis against the PET estimate and,
# if b0 is significant, the PEESE estimate.
# A funnel plot and regresion lines are provided.
# You can force it to return PEESE regardless of sig. b0 with
  # alwaysPEESE = T

# To inspect influence statistics for outliers, etc., 
# Try plot(influence(PET(...))) or plot(influence(PEESE(...)))

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
               data=dataset,
               method = "FE")
  return(petOut)
}

# PEESE
PEESE=function(dataset) {
  peeseOut = rma(yi = Fisher.s.Z, 
                 sei = Std.Err, 
                 mods = ~I(Std.Err^2), 
                 data=dataset,
                 method = "FE")
  return(peeseOut)
}

# make a funnel plot with PET line and conditional PEESE line
funnelPETPEESE = function(dataset, 
                          alwaysPEESE=F, plotName=NULL, ...) {
  naiveModel = naive(dataset)
  petModel = PET(dataset)
  peeseModel = PEESE(dataset)
  # make funnel plot
  funnel(naiveModel)
  title(plotName, line=3)
  naiveModel$b[1] %>% 
    tanh %>% 
    round(3) %>%
    paste("Naive meta estimate, r =", .) %>%
    mtext(side=1)
  # add line and text from PET
  petModel %$% 
    abline(a = -b[1]/b[2], b = 1/b[2])
  r = petModel$b[1] %>% tanh %>% round(3)
  p.effect = petModel$pval[1] %>% round(3)
  p.bias = petModel$pval[2] %>% round(3)
  mtext(paste("PET r = ", r
              , ", p-effect = ", p.effect
              , ", p-bias = ", p.bias
              , sep=""))
  points(x = petModel$b[1], y=0, cex=1.5)
  # add line and text from PEESE
  if(petModel$pval[1] < .05 || alwaysPEESE == T) {
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
      tanh %>%
      round(3) %>%
      paste("PEESE r =", .) %>%
      mtext(line = 1)
  }
}

# Leave-one-out sensitivity analysis ----
  # I felt it necessary to do this b/c influence.rma.uni
  # gives DFBETAs but those don't translate nicely to actual coefficients
  # (I don't know what the standard deviation of betas are)
# I'm gonna write it in a loop instead of lapply() or whatever. Screw it.
sensitivityPETPEESE = function(dataset) {
  sensitivityFrame = data.frame("Study.name" = NULL,
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
                                "PEESE.b1.p" = NULL,
                                # Other ID
                                "Full.Reference" = NULL)
  for (i in 1:nrow(dataset)) {
    modelPET = PET(dataset[-i,])
    modelPEESE = PEESE(dataset[-i,])
    output = data.frame(
      # ID data
      "Study.name" = dataset$Study.name[i],
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
      "PEESE.b1.p" = modelPEESE$pval[2],
      # Other identifiers
      "Full.Reference" = dataset$Full.Reference[i]
    )
    sensitivityFrame = rbind(sensitivityFrame, output)
  }
  # Conditional estimator
  sensitivityFrame$PETPEESE.r = ifelse(sensitivityFrame$PET.b0.p < .05,
                                       tanh(sensitivityFrame$PEESE.b0),
                                       tanh(sensitivityFrame$PET.b0))
  # Could then add cook's d or dfbetas to sensitivityFrame using cbind()
  sensitivityFrame$PET.dfbeta.0 = influence(PET(dataset))$dfb[,1]
  sensitivityFrame$PEESE.dfbeta.0 = influence(PEESE(dataset))$dfb[,1]
  return(sensitivityFrame)
}
  
# install.packages(c('metafor', 'magrittr'))
library(metafor)
library(tidyverse)

# I'm terrible with functions and global/local environment so
  # these functions simply assume your columns have names like
  # Comprehensive Meta-Analysis v3 gives them, e.g.
  # yi is "Fisher.s.Z",
  # sei is "Std.Err"
# If this assumption is wrong, please rename your columns
#   or change this code.
# If you're working in d instead of r, remove the tanh steps

# Of these functions, the most useful is probably funnelPETPEESE()
# This compares the naive meta-analysis against the PET estimate and,
# if b0 is significant, the PEESE estimate.
# A funnel plot and regresion lines are provided.
# You can force it to return PEESE regardless of sig. b0 with
  # alwaysPEESE = T

# To inspect influence statistics for outliers, etc., 
# Try plot(influence(PET(...))) or plot(influence(PEESE(...)))

# naive fixed-effects meta-analysis ----
naive = function(dataset, ...) {
  rma(yi = Fisher.s.Z,
      sei = Std.Err,
      data = dataset,
      method = "FE",
      ...)
}

naiveRE = function(dataset, ...) {
  rma(yi = Fisher.s.Z,
      sei = Std.Err,
      data = dataset,
      method = "REML",
      ...)
}

# basic PET ----
PET=function(dataset, error = "additive", ...) {
  if (error == "additive") {
    petOut = rma(yi = Fisher.s.Z, 
                 sei = Std.Err, 
                 mods = ~Std.Err, 
                 data = dataset,
                 method = "REML", 
                 control = list(stepadj = .5), # reduced step length to help convergence
                 ...)
  }
  if (error == "multiplicative") {
    petOut = lm(Fisher.s.Z ~ Std.Err,
                weights = 1/Std.Err,
                data=dataset,
                ...)
  }
    return(petOut)
}

# basic PEESE ----
PEESE=function(dataset, error = "additive", ...) {
  if (error == "additive") {
    peeseOut = rma(yi = Fisher.s.Z, 
                   sei = Std.Err, 
                   mods = ~I(Std.Err^2), 
                   data=dataset,
                   method = "REML",
                   control = list(stepadj = .5), # reduced step length to help convergence
                   ...)
  }
  if (error == "multiplicative") {
    peeseOut = lm(Fisher.s.Z ~ I(Std.Err^2), 
                  weights = 1/Std.Err,
                  data = dataset,
                  ...)
  }
    return(peeseOut)
}

# funnel plot with PET line and conditional PEESE line ----
funnelPETPEESE = function(dataset, 
                          error = "additive",
                          alwaysPEESE=T, plotName=NULL, printText = T,
                          ...) {
  naiveModel = naive(dataset)
  petModel = PET(dataset, error)
  peeseModel = PEESE(dataset, error)
  # make funnel plot
  funnel(naiveModel, ...)
  if (printText == T) title(plotName, line=3)
  if (printText == F) title(plotName)
  if (error == "additive") {
    if (printText == T) naiveModel$b[1] %>% 
      tanh %>% 
      round(3) %>%
      paste("Naive meta estimate, r =", .) %>%
      mtext(side=1)
    # add line and text from PET
    with(petModel, abline(a = -b[1]/b[2], b = 1/b[2]))
    r = petModel$b[1] %>% tanh %>% round(3)
    p.effect = petModel$pval[1] %>% round(3)
    p.bias = petModel$pval[2] %>% round(3)
    if (printText == T) mtext(paste("PET r = ", r
                                    , ", p-effect = ", p.effect
                                    , ", p-bias = ", p.bias
                                    , sep=""))
    points(x = petModel$b[1], y=0, cex=1.5)
    #abline(v = petModel$b[1], lty = 2)
    #lines(x = rep(petModel$b[1], 2), y = c(ymin, ymin - .1)
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
      with(grid, lines(x=Fisher.s.Z, y=Std.Err, typ='l'))
      points(x = (peeseModel$b[1]), y=0, cex=1.5, pch=5)
      #abline(v = peeseModel$b[1], lty = 2)
      if (printText == T) { 
        peeseModel$b[1] %>%
          tanh %>%
          round(3) %>%
          paste("PEESE r =", .) %>%
          mtext(line = 1)
      }
    }
  }
  if(error == "multiplicative") {
    if (printText == T) naiveModel$b[1] %>% 
      tanh %>% 
      round(3) %>%
      paste("Naive meta estimate, r =", .) %>%
      mtext(side=1)
    # add line and text from PET
    b = summary(petModel)$coefficients[,1]
    with(petModel, abline(a = -b[1]/b[2], b = 1/b[2]))
    r = b[1] %>% tanh %>% round(3)
    p.effect = summary(petModel)$coefficients[1,4] %>% round(3)
    p.bias = summary(petModel)$coefficients[2,4] %>% round(3)
    if (printText == T) mtext(paste("PET r = ", r
                                    , ", p-effect = ", p.effect
                                    , ", p-bias = ", p.bias
                                    , sep=""))
    points(x = b[1], y=0, cex=1.5)
    #abline(v = b[1], lty = 2)
    # add line and text from PEESE
    if(summary(petModel)$coefficients[1,4] < .05 || alwaysPEESE == T) {
      grid = 
        naiveModel$vi %>%
        raise_to_power(.5) %>%
        max %>%
        seq(0, ., .001) %>%
        data.frame("Std.Err" = .)
      grid$Var = grid$Std.Err^2
      b = summary(peeseModel)$coefficients[,1]
      grid$Fisher.s.Z = 
        b[1] + b[2]*grid$Var
      with(grid, lines(x=Fisher.s.Z, y=Std.Err, typ='l'))
      points(x = b[1], y=0, cex=1.5, pch=5)
      #abline(v = b[1], lty = 2)
      if (printText == T) {
        b[1] %>%
          tanh %>%
          round(3) %>%
          paste("PEESE r =", .) %>%
          mtext(line = 1)
      }
    }
  }
}

# # 95% CI for I2 statistic [MOTHBALLED -- USE confint() INSTEAD]
# # Based on equations 16-18 in Huedo-Medina et al. 2006
# CI_I2 = function(model) {
#   # Get stats from model object
#   Q = model$QE
#   k = model$k
#   H = sqrt(model$H2)
#   # Calculate SE of log(H) (eqtn 18)
#   lnH.se = ifelse(Q > k,
#                 1/2 * (log(Q) - log(k-1)) / (sqrt(2*Q) - sqrt(2*k - 3)), # if Q > k
#                 sqrt(1/(2*(k-2)) * (1 - (1 / (3 * (k-2)^2)))) # if Q <= k
#   )
#   # Use SE to make upper & lower bound of 95% CI on log(H) (eqtn 17)
#   lnH.lb = log(H) + qnorm(.025) * lnH.se
#   lnH.ub = log(H) + qnorm(.975) * lnH.se
#   # Convert log(H) to H 
#   H.lb = exp(lnH.lb)
#   H.ub = exp(lnH.ub)
#   # Use H to get I^2 (eqtn 16)
#   I2.lb = (H.lb^2 - 1)/H.lb^2
#   I2.lb = max(c(0, I2.lb))
#   I2.ub = (H.ub^2 - 1)/H.ub^2
#   return(c(I2.lb, I2.ub))
# }

# Leave-one-out sensitivity analysis ----
  # I felt it necessary to do this b/c influence.rma.uni
  # gives DFBETAs but those don't translate nicely to actual coefficients
  # (I don't know what the standard deviation of betas are)
# I'm gonna write it in a loop instead of lapply() or whatever. Screw it.
sensitivityPETPEESE = function(dataset, error = "additive") {
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
    modelNaiveFE = rma(yi = Fisher.s.Z, sei = Std.Err, method = "FE", data = dataset[-i, ])
    modelNaiveRE = rma(yi = Fisher.s.Z, sei = Std.Err, method = "REML", data = dataset[-i, ],
                       control = list(stepadj = .5))
    modelPET = PET(dataset[-i,])
    modelPEESE = PEESE(dataset[-i,])
    if (error == "additive") {
      output = data.frame(
      # ID data
      "Study.name" = dataset$Study.name[i],
      # Naive stats
      "naive-FE.r" = tanh(modelNaiveFE$b[1]),
      "naive-FE.r.lb" = tanh(modelNaiveFE$ci.lb[1]),
      "naive-FE.r.ub" = tanh(modelNaiveFE$ci.ub[1]),
      "naive-RE.r" = tanh(modelNaiveRE$b[1]),
      "naive-RE.r.lb" = tanh(modelNaiveRE$ci.lb[1]),
      "naive-RE.r.ub" = tanh(modelNaiveRE$ci.ub[1]),
      # PET stats
      "PET.b0" = modelPET$b[1],
      "PET.b0.se" = modelPET$se[1],
      "PET.b0.p" = modelPET$pval[1],
      "PET.b1" = modelPET$b[2],
      "PET.b1.se" = modelPET$se[2],
      "PET.b1.p" = modelPET$pval[2],
      "PET.r" = tanh(modelPET$b[1]),
      "PET.r.lb" = tanh(modelPET$ci.lb[1]),
      "PET.r.ub" = tanh(modelPET$ci.ub[1]),
      # PEESE stats
      "PEESE.b0" = modelPEESE$b[1],
      "PEESE.b0.se" = modelPEESE$se[1],
      "PEESE.b0.p" = modelPEESE$pval[1],
      "PEESE.b1" = modelPEESE$b[2],
      "PEESE.b1.se" = modelPEESE$se[2],
      "PEESE.b1.p" = modelPEESE$pval[2],
      "PEESE.r" = tanh(modelPEESE$b[1]),
      "PEESE.r.lb" = tanh(modelPEESE$ci.lb[1]),
      "PEESE.r.ub" = tanh(modelPEESE$ci.ub[1]),
      # Other identifiers
      "Full.Reference" = dataset$Full.Reference[i]
    )
  }
    if (error == "multiplicative") {
      output = data.frame(
        # ID data
        "Study.name" = dataset$Study.name[i],
        # PET stats
      "PET.b0" = summary(modelPET)$coefficients[1,1],
      "PET.b0.se" = summary(modelPET)$coefficients[1,2],
      "PET.b0.p" = summary(modelPET)$coefficients[1,4],
      "PET.b1" = summary(modelPET)$coefficients[2,1],
      "PET.b1.se" = summary(modelPET)$coefficients[2,2],
      "PET.b1.p" = summary(modelPET)$coefficients[2,4],
      # PEESE stats
      "PEESE.b0" = summary(modelPEESE)$coefficients[1,1],
      "PEESE.b0.se" = summary(modelPEESE)$coefficients[1,2],
      "PEESE.b0.p" = summary(modelPEESE)$coefficients[1,4],
      "PEESE.b1" = summary(modelPEESE)$coefficients[2,1],
      "PEESE.b1.se" = summary(modelPEESE)$coefficients[2,2],
      "PEESE.b1.p" = summary(modelPEESE)$coefficients[2,4],
      # Other identifiers
      "Full.Reference" = dataset$Full.Reference[i]
      )
    }
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
  

# Peters meta-regression (PET-like function) ----
peters_linear = function(dataset) {
  petersOut = rma(yi = Fisher.s.Z,
                  sei = Std.Err,
                  mods = ~I(1/sqrt(Sample.size)),
                  data = dataset,
                  method = "FE")
}

# Peters meta-regression (PEESE-like function) ----
peters_quadratic = function(dataset) {
  petersOut = rma(yi = Fisher.s.Z,
                  sei = Std.Err,
                  mods = ~I(1/Sample.size),
                  data = dataset,
                  method = "FE")
}

# funnel plot with PET and PEESE-like Peters estimates ----
funnelPeters = function(dataset, 
                          alwaysPEESE=T, plotName=NULL, ...) {
  naiveModel = naive(dataset)
  petModel = peters_linear(dataset)
  peeseModel = peters_quadratic(dataset)
  # make funnel plot
  funnel(naiveModel)
  title(plotName, line=3)
  naiveModel$b[1] %>% 
    tanh %>% 
    round(3) %>%
    paste("Naive meta estimate, r =", .) %>%
    mtext(side=1)
  # add line and text from PET
  with(petModel, abline(a = -b[1]/b[2], b = 1/b[2]))
  r = petModel$b[1] %>% tanh %>% round(3)
  p.effect = petModel$pval[1] %>% round(3)
  p.bias = petModel$pval[2] %>% round(3)
  mtext(paste("Linear r = ", r
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
    with(grid, lines(x=Fisher.s.Z, y=Std.Err, typ='l'))
    points(x = (peeseModel$b[1]), y=0, cex=1.5, pch=5)
    peeseModel$b[1] %>%
      tanh %>%
      round(3) %>%
      paste("Quadratic r =", .) %>%
      mtext(line = 1)
  }
}


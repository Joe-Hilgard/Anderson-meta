# demo script of pub bias adjustments and what they look like

# back-selection function and examples ----
# Select a number of published studies and sample statistics according
# to effect size, n, and bias.
library(truncdist) # for sampling biased studies
library(magrittr) # for piping
library(metafor) # for meta-analysis

# My funnel plot style
source("PETPEESE_functions.R")
myFunnel = function(set, ...) {
  funnelPETPEESE(set, printText = F,
                 back = NULL, hlines = "grey80", 
                 pch = ifelse(set$sig, 16, 1),
                 ...)
}

# function for generating k studies of some effect, at least X percent of which are significant
gen_k_studies = function(k, percent_sig, avg_n, 
                         min_n = 15, max_n = 300, sdlog = .5,
                         d_true=0,
                         tails = 2, force_nonsig = F) {
  
  if (avg_n == min_n && avg_n == max_n) {
    n_per_cell <- avg_n
  } else {
    n_per_cell = 
      rtrunc(k, "lnorm", a = min_n, b = max_n, 
             meanlog = log(avg_n), sdlog = sdlog) %>% round(0)    	
  }
  df = n_per_cell * 2 - 2
  
  # Run studies
  # minimum significant t-value
  if (tails == 1) {
    tcrit = qt(.95, df)
  }  else if (tails == 2) tcrit = qt(.975, df)
  # make a vector 'lower_bound' that is equal to
  # -Inf if the study is not necessarily significant,
  # tcrit if the study must be significant.
  sig = rbinom(k, size = 1, prob = percent_sig) + 1 # 1 if false, 2 if true
  lower_bound = c(-Inf, 1)[sig]
  lower_bound = lower_bound * tcrit
  # Ditto 'upperbound'. 
  # Default: force_nonsig == T, Nonsig studies not allowed to have sig result.
  if (force_nonsig == T) {
    upper_bound = c(1, Inf)[sig]
    upper_bound = upper_bound * tcrit
  }
  # Optional: force_nonsig == F, Nonsig studies may achieve sig as power & chance permit
  if (force_nonsig == F) {
    upper_bound = c(Inf, Inf)[sig]
    upper_bound = upper_bound * tcrit
  }
  # sample a t-value coerced to be significant or nonsignificant.
  #rtrunc() barks but seems to work fine
  t = rtrunc(k, "t", a = lower_bound, b = upper_bound,
             df = df,
             ncp = sqrt(n_per_cell/2)*d_true) # correct equation for ncp?
  d=2*t/sqrt(2*n_per_cell)
  var_d = ( (2*n_per_cell / (n_per_cell^2)) + d^2 / (2*df) ) * (2*n_per_cell / df)
  se_d = sqrt(var_d)
  p=2*pt(t, df=df, lower.tail = F) %>% round(3)
  # convert to r, z for convenience
  # per Borenstein, Hedges, Higgins & Rothstein (2009) textbook, Ch 7.
  a = ((n_per_cell*2)^2) / (n_per_cell^2) # a will be 4 so long as we assume n1=n2
  r = d / sqrt(d^2 + a)
  z = atanh(r)
  se_z = 1/sqrt(n_per_cell*2 - 3)
  var_z = se_z^2
  # output
  out = data.frame("Study" = 1:k, "nobs" = n_per_cell, 
                   "dobs" = d, "sedobs" = se_d, "vardobs" = var_d,
                   "robs" = r, "zobs" = z, "sezobs" = se_z, "varzobs" = var_z,
                   "t" = t, "p" = p)
}

# PET and PEESE seem to perform terribly in many of these demos
# maybe not enough variabilty in sample size?
set.seed(42069)
# d_true = .4
# (r_true = d_true/sqrt(d_true^2 + 4))
# (z_true = atanh(r_true))
z_true = .2
r_true = tanh(z_true)
d_true = (2*r_true)/sqrt(1 - r_true^2) # formula from Borenstein et al. textbook
# generate datasets: unbiased|H1, biased|H1, biased|H0
unbiased = gen_k_studies(20, 0, 40, sdlog = .5, force_nonsig = F, d_true = d_true)
biased = gen_k_studies(20, .80, 40, sdlog = .5, force_nonsig = F, d_true = d_true)
biased.null = gen_k_studies(20, .80, 40, sdlog = .5, force_nonsig = F, d_true = 0)
# fit meta-analyses
unbiased.model = rma(yi = zobs, sei = sezobs, ni = nobs, data = unbiased)
biased.model = rma(yi = zobs, sei = sezobs, ni = nobs, data = biased)
biased.null.model = rma(yi = zobs, sei = sezobs, ni = nobs, data = biased.null)
# make sig column
unbiased$sig = unbiased$p < .05
biased$sig = biased$p < .05
biased.null$sig = biased.null$p < .05

# prepare .pdf export
pdf("funnels_1.pdf", width = 5, height = 6)
# prepare faceting
par(mfrow = c(3,2),
    mar = c(4, 4, 2, 1) + .01)
# funnel plots
funnel(unbiased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)"#, pch = ifelse(unbiased$sig, 16, 1)
       )
abline(v = z_true, lty = 2)
abline(v = unbiased.model$b, lty = 3)
mtext(side=3,adj=.05,cex=1.5,'A.',line=-1.5)
funnel(biased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)"#, pch = ifelse(biased$sig, 16, 1)
       )
abline(v = z_true, lty = 2)
abline(v = biased.model$b, lty = 3)
mtext(side=3,adj=.05,cex=1.5,'B.',line=-1.5)

# trim and fill
funnel(trimfill(unbiased.model), xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)")
abline(v = z_true, lty = 2)
abline(v = trimfill(unbiased.model)$b, lty = 3)
mtext(side=3,adj=.05,cex=1.5,'C.',line=-1.5)
funnel(trimfill(biased.model), xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)")
abline(v = z_true, lty = 2)
abline(v = trimfill(biased.model)$b, lty = 3)
mtext(side=3,adj=.05,cex=1.5,'D.',line=-1.5)

# Egger test
# additive (not multiplicative) error in meta-regression
funnel(unbiased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)"#, pch = ifelse(unbiased$sig, 16, 1)
       )
unbiased.egger = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = unbiased)
unbiased.egger.coefs = summary(unbiased.egger)$b
b = unbiased.egger.coefs
abline(a = -b[1]/b[2], b = 1/b[2])
abline(v = z_true, lty = 2)
abline(v = unbiased.model$b, lty = 3)
mtext(side=3,adj=.05,cex=1.5,'E.',line=-1.5)

funnel(biased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)"#, pch = ifelse(biased$sig, 16, 1)
       )
biased.egger = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = biased)
biased.egger.coefs = summary(biased.egger)$b
b = biased.egger.coefs
abline(a = -b[1]/b[2], b = 1/b[2])
abline(v = z_true, lty = 2)
abline(v = biased.model$b, lty = 3)

mtext(side=3,adj=.05,cex=1.5,'F.',line=-1.5)

dev.off()

pdf("funnels_2.pdf", width = 11, height = 8)
par(mfrow = c(2,3))

# PET estimate
funnel(unbiased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(unbiased$sig, 16, 1))
unbiased.egger = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = unbiased)
unbiased.egger.coefs = summary(unbiased.egger)$b
b = unbiased.egger.coefs
abline(a = -b[1]/b[2], b = 1/b[2])
abline(v = z_true, lty = 2)
points(x = b[1], y = 0, pch = 1)
abline(v = b[1], lty = 3)
mtext(side=3,adj=.05,cex=1.5,'A.',line=-1.5)

funnel(biased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(biased$sig, 16, 1))
biased.egger = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = biased)
biased.egger.coefs = summary(biased.egger)$b
b = biased.egger.coefs
abline(a = -b[1]/b[2], b = 1/b[2])
abline(v = z_true, lty = 2)
points(x = b[1], y = 0, pch = 1)
abline(v = b[1], lty = 3)
mtext(side=3,adj=.05,cex=1.5,'B.',line=-1.5)

funnel(biased.null.model, xlim = c(0 - .5, 0 + .5),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(biased.null$sig, 16, 1))
biased.null.egger = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = biased.null)
biased.null.egger.coefs = summary(biased.null.egger)$b
b = biased.null.egger.coefs
abline(a = -b[1]/b[2], b = 1/b[2])
abline(v = 0, lty = 2)
points(x = b[1], y = 0, pch = 1)
abline(v = b[1], lty = 3)
mtext(side=3,adj=.05,cex=1.5,'C.',line=-1.5)

# PEESE estimate
funnel(unbiased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(unbiased$sig, 16, 1))
unbiased.PEESE = rma(yi=zobs, sei=sezobs, mods = ~varzobs, data = unbiased)
unbiased.PEESE.coefs = summary(unbiased.PEESE)$b
b = unbiased.PEESE.coefs
grid = 
  unbiased.model$vi %>%
  raise_to_power(.5) %>%
  max %>%
  seq(0, ., .001) %>%
  data.frame("Std.Err" = .)
grid$Var = grid$Std.Err^2
grid$zobs = 
  b[1] + b[2]*grid$Var
grid %$% lines(x=zobs, y=Std.Err, typ='l')
points(x = b[1], y=0, cex=1.5, pch=5)
abline(v = b[1], lty = 3)
abline(v = z_true, lty = 2)
mtext(side=3,adj=.05,cex=1.5,'D.',line=-1.5)

funnel(biased.model, xlim = c(z_true - .4, z_true + .4),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(biased$sig, 16, 1))
biased.PEESE = rma(yi=zobs, sei=sezobs, mods = ~varzobs, data = biased)
biased.PEESE.coefs = summary(biased.PEESE)$b
b = biased.PEESE.coefs
grid = 
  biased.model$vi %>%
  raise_to_power(.5) %>%
  max %>%
  seq(0, ., .001) %>%
  data.frame("Std.Err" = .)
grid$Var = grid$Std.Err^2
grid$zobs = 
  b[1] + b[2]*grid$Var
grid %$% lines(x=zobs, y=Std.Err, typ='l')
points(x = b[1], y=0, cex=1.5, pch=5)
abline(v = b[1], lty = 3)
abline(v = z_true, lty = 2)
mtext(side=3,adj=.05,cex=1.5,'E.',line=-1.5)

funnel(biased.null.model, xlim = c(0 - .5, 0 + .5),
       #level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0)
       back = NULL, hlines = "grey80", xlab = "Observed Effect Size (z)", pch = ifelse(biased.null$sig, 16, 1))
biased.null.PEESE = rma(yi=zobs, sei=sezobs, mods = ~sezobs, data = biased.null)
biased.null.PEESE.coefs = summary(biased.null.PEESE)$b
b = biased.null.PEESE.coefs
grid = 
  biased.null.model$vi %>%
  raise_to_power(.5) %>%
  max %>%
  seq(0, ., .001) %>%
  data.frame("Std.Err" = .)
grid$Var = grid$Std.Err^2
grid$zobs = 
  b[1] + b[2]*grid$Var
grid %$% lines(x=zobs, y=Std.Err, typ='l')
points(x = b[1], y=0, cex=1.5, pch=5)
abline(v = b[1], lty = 3)
abline(v = 0, lty = 2)
mtext(side=3,adj=.05,cex=1.5,'F.',line=-1.5)

dev.off()

# p-curve
hist(unbiased$p[unbiased$p < .05], breaks = seq(0, .05, .01),
     xlab = "p-value", main = "Unbiased Literature p-curve")

hist(biased$p[biased$p < .05], breaks = seq(0, .05, .01),
     xlab = "p-value", main = "Biased Literature p-curve")

# # Demonstration ----
# # Generate 20 significant results on a null effect
# biased_meta = gen_k_studies(k = 20, percent_sig = 1, avg_n = 30, d_true = 0)
# plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .7))
# subset(biased_meta$p, biased_meta$p < .05) %>%
#   hist(breaks=seq(0, .05, by=.01))
# # Generate 30 studies on a null effect, of which 75% are forced to be significant
# biased_meta = gen_k_studies(k = 30, percent_sig = .75, avg_n = 30, d_true = 0)
# plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .7))
# subset(biased_meta$p, biased_meta$p < .05) %>%
#   hist(breaks=seq(0, .05, by=.01))
# # Generate 40 studies on a small effect with heavy pub bias
# biased_meta = gen_k_studies(k = 40, percent_sig = .80, avg_n = 30, d_true = .3)
# plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .9))
# subset(biased_meta$p, biased_meta$p < .05) %>%
#   hist(breaks=seq(0, .05, by=.01))


# Test of excess significance
library(pwr)
library(dplyr)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat$sig = ifelse(dat$p.twotail < .05, "significant", "not-significant")

# From Ioannidis & Trikalinos, p. 246
# Expected # of sig results is sum of studies' powers
# (We use meta-analytic estimate r = .21 as hypothesized ES)
TES = function(dataset, r) {
  # generate power estimates
  for (i in 1:nrow(dataset)) {
    dataset$power[i] = pwr.r.test(n=dataset$Sample.size[i],
                                  r=r,
                                  alternative = "two.sided")$power
  }
  plot(dataset$Sample.size, dataset$power)  
  # Conduct TES
  n = nrow(dataset) # n is number of studies
  E = sum(dataset$power) # E is expected # of sig results, sum of power
  O = sum(dataset$sig == "significant") # O is observed # of sig results
  # Statistic is 
  A = (O-E)^2/E + (O-E)^2/(n-E)
  # Test A for statistical significance
  pchisq(A, 1, lower.tail = F) %>% 
    round(3) %>% 
    return()
}

# Make output data frame
out = expand.grid("Best" = c("Best", "Full"),
                  "Outcome" = c("AggAff", "AggBeh", "AggCog", "PhysArous"),
                  "Setting" = c("Exp", "NonExp"))
# Sort and clean it
out = out %>% 
  filter(!(Outcome == "PhysArous" & Setting == "NonExp")) %>% 
  select(Setting, Outcome, Best)
# TODO: Make triple loop to run all analyses, sensitivity analyses.



# Aggressive behavior ----
# Experiments, best-practices 
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. == "y") %>% 
  TES(r = .210)
# significant, p = .016

# Experiments, all
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh") %>% 
  TES(r = .181)
# significant, p = .058

# Cross-section, best-practices
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggBeh", Best. == "y") %>% 
  TES(r = .262)
# not significant, p = .916

# Cross-section, all
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggBeh") %>% 
  TES(r = .189)
# not significant, p = .274

# Aggressive Cognition ----
# Experiments, best-practices
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. == "y") %>% 
  TES(r = .217)
# not significant, p = .550

# Experiments, all
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog") %>% 
  TES(r = .207)
# not significant, p = .791

# Cross-section, best-practices
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggCog", Best. == "y") %>% 
  TES(r = .183)
# not significant, p = .568

# Cross-section, all
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggCog") %>% 
  TES(r = .164)
# not significant, p = .278

# Aggressive affect ----
# Experiments, best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y") %>% 
  TES(r = .294)
# significant, p = .096

# Experiments, all
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff") %>% 
  TES(r = .181)
# significant, p = .048

# Cross-section, best-practices
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggAff", Best. == "y") %>% 
  TES(r = .101)
# not significant, p = .915

# Cross-section, all
dat %>% 
  filter(Setting == "Nonexp", Outcome == "AggAff") %>% 
  TES(r = .145)
# not significant, p = .253
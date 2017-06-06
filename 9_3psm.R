# 3-parameter selection model
library(weightr)
library(tidyverse)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat <- mutate(dat, vi = Std.Err^2)


# Can't loop because of fe = F, fe = T issues

# Experiments ----
# Affect
dat %>% filter(Outcome == "AggAff", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) # r = .11
# significant selection parameter

dat %>% filter(Outcome == "AggAff", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, steps = c(.025, 1), table = T)) #r = .17
# no p > .10, had to change steps

# Exclude outlier from best-practices
dat %>% 
  filter(Outcome == "AggAff", Setting == "Exp", Best. == "y") %>% 
  filter(Study.name != "BW96AA") %>% 
  with(., weightfunct(Fisher.s.Z, vi, steps = c(.025, 1), table = T)) #r = .19
# and a lot of error messages

# Behavior
dat %>% filter(Outcome == "AggBeh", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T, table = T)) # RFX won't converge # r = .12
# note significant selection parameter

dat %>% filter(Outcome == "AggBeh", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T, table = T)) # RFX won't converge # r = .15
# note significant selection parameter
# note rarity of p > .05
# Could it be selection model is misspecified?

# Cognition
dat %>% filter(Outcome == "AggCog", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi))  # r = .15
# note significant selection parameter

dat %>% filter(Outcome == "AggCog", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) # r = .10
# note significant selection parameter

# Cross-sections ----
# Affect
dat %>% filter(Outcome == "AggAff", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi, table = T)) # r = .09

dat %>% filter(Outcome == "AggAff", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, table = T)) # r = .11

# Behavior
dat %>% filter(Outcome == "AggBeh", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi, table = T)) # r = .22

dat %>% filter(Outcome == "AggBeh", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, table = T)) # r = .30

# Cognition
dat %>% filter(Outcome == "AggCog", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi, table = T)) # r = .12
# note significant selection parameter

dat %>% filter(Outcome == "AggCog", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T, table = T)) # r = .21
# RFX won't converge
# note significant selection parameter
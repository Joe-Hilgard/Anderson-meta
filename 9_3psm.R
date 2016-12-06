# 3-parameter selection model
library(weightr)
library(tidyverse)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat <- mutate(dat, vi = Std.Err^2)


# Can't loop because of fe = F, fe = T issues

# Experiments ----
# Affect
dat %>% filter(Outcome == "AggAff", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi))
# significant selection parameter

dat %>% filter(Outcome == "AggAff", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, steps = c(.025, 1))) 
# no p > .10, had to change steps
# significant selection parameter

# Behavior
dat %>% filter(Outcome == "AggBeh", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T)) # RFX won't converge
# note significant selection parameter

dat %>% filter(Outcome == "AggBeh", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T)) # RFX won't converge
# note significant selection parameter

# Cognition
dat %>% filter(Outcome == "AggCog", Setting == "Exp") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) 
# note significant selection parameter

dat %>% filter(Outcome == "AggCog", Setting == "Exp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi))
# note significant selection parameter

# Cross-sections ----
# Affect
dat %>% filter(Outcome == "AggAff", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi))

dat %>% filter(Outcome == "AggAff", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) 

# Behavior
dat %>% filter(Outcome == "AggBeh", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) 

dat %>% filter(Outcome == "AggBeh", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) 

# Cognition
dat %>% filter(Outcome == "AggCog", Setting == "Nonexp") %>% 
  with(., weightfunct(Fisher.s.Z, vi)) 
# note significant selection parameter

dat %>% filter(Outcome == "AggCog", Setting == "Nonexp", Best. == "y") %>% 
  with(., weightfunct(Fisher.s.Z, vi, fe = T)) # RFX won't converge
# note significant selection parameter
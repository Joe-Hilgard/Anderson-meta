# Counts (and averages) of adequately-powered studies

library(tidyverse)
library(metafor)

# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# AggBeh, Exp, Best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 136
# Two-tailed needs N = 173
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. == "y",
         Sample.size >= 136) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Five studies are adequately powered for one tail, r = .16
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. == "y",
         Sample.size >= 173) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Three studies are adequately powered for two tails, r = .15

# AggCog, Exp, Best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 123
# Two-tailed needs N = 157
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. == "y",
         Sample.size >= 123) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Ten studies are adequately powered for one tail, r = .20
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. == "y",
         Sample.size >= 157) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Seven studies are adequately powered for two tails, r = .22

# AggAff, Exp, Best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 64
# Two-tailed needs N = 82
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y",
         Sample.size >= 64) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Ten studies are adequately powered for one tail, r = .23
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y",
         Sample.size >= 82) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Eight studies are adequately powered for two tails, r = .23

# Was Stanley's comment based on the full sample, rather than best-only?
# AggBeh, Exp, All
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 210
# Two-tailed needs N = 266
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh",
         Sample.size >= 210) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Two studies are adequately powered for one tail, r = .13
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh",
         Sample.size >= 266) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One study is adequately powered for two tails, r = .13

# AggCog, Exp, Best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 150
# Two-tailed needs N = 191
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog",
         Sample.size >= 150) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Nine studies are adequately powered for one tail, r = .20
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog",
         Sample.size >= 191) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# Seven studies are adequately powered for two tails, r = .21

# AggAff, Exp, Best
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# One-tailed needs N = 198
# Two-tailed needs N = 251
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y",
         Sample.size >= 198) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, data = ., method = "FE")
# No studies are adequately powered
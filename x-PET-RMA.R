# Additional PET-RMA analyses

# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

## Read in PET-PEESE functions
source("PETPEESE_functions.R")
# get dplyr package for distinct()
library(dplyr)

# Significant moderation of Best?
dat %>% 
  filter(Setting == "Exp" & Outcome == "AggBeh") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err * Best., data = .) # no?

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggBeh") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) * Best., data = .) # no?

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggAff") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err * Best., data = .) # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggAff") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) * Best., data = .) # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggCog") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err * Best., data = .) # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggCog") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) * Best., data = .) # yes

# The adults vs children hypothesis
# WORK IN PROGRESS
# not sure whether to test in full sample or best-practices subset
# Consider also that it would be helpful to make funnel plots w/
#  pch coding for the age group

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggBeh" & 
           AGE %in% c("Adult", "Child")) %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err + AGE, data = .,
      subset = Best. == "y") # no?

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggBeh" & 
           AGE %in% c("Adult", "Child")) %>%  
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) + AGE, data = .,
      subset = Best. == "y") # no?

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggAff") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err + AGE, data = .,
      subset = Best. == "y") # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggAff") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) + Best., 
      data = .) # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggCog") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ Std.Err * Best., data = .) # yes

dat %>% 
  filter(Setting == "Exp" & Outcome == "AggCog") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, 
      mods = ~ I(Std.Err^2) * Best., data = .) # yes
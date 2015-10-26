# Read in PET-PEESE functions
source("PETPEESE_functions.R")
library(dplyr)
library(readxl)

# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat2 = read_excel("CRTT.xlsx")
dat = left_join(dat, dat2, by = c("Study.name", "Study", "Full.Reference"))

dat.aggBeh = 
  dat %>% 
  filter(Outcome == "AggBeh",
         Setting == "Exp") 
model = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh)
model
model.best = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh, 
                 subset = Best. == "y")
model.best

crtt.best = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh,
                subset = Best. %in% "y" & CRTT %in% "Yes",
                method = "FE")
other.best = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh,
                 subset = Best. %in% "y" & !(CRTT %in% "Yes"),
                 method = "FE")
crtt.all = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh,
               subset = CRTT %in% "Yes",
               method = "FE")
other.all = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh,
                subset = !(CRTT %in% "Yes"),
                method = "FE")
crtt.best
other.best
crtt.all
other.all


funnel(model, 
       pch = ifelse(dat.aggBeh$CRTT %in% "Yes", 16, 1),
       cex = ifelse(dat.aggBeh$CRTT %in% "Yes", 1, .5),
       refline = 0, level = c(90, 95, 99), shade = c("white", "gray95", "gray90"))

tapply(dat.aggBeh$Sample.size, dat.aggBeh$CRTT, FUN = mean)

egger.crtt.all = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err,
                     data = dat.aggBeh,
                     subset = CRTT %in% "Yes")
egger.other.all = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err,
                      data = dat.aggBeh,
                      subset = !(CRTT %in% "Yes"))
egger.crtt.all
egger.other.all

egger.crtt.all = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err,
                     data = dat.aggBeh,
                     subset = CRTT %in% "Yes" & Study.name != "FRC08AB1e")
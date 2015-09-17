# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

## Read in PET-PEESE functions
source("PETPEESE_functions.R")
# get dplyr package for distinct()
library(dplyr)


png("Anderson_funnels_1.png", width = 5, height = 6, units = 'in', res = 280)
# prepare faceting
par(mfrow = c(3,2),
    mar = c(2, 4, 2, 1) + .01)

dat %>% 
  filter(dat$Outcome == "AggAff" & dat$Setting == "Exp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Affect, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggAff" & dat$Setting == "Exp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Affect, All Experiments", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggAff" & dat$Setting == "Exp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)

dat %>% 
  filter(dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Behavior, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Behavior, All Experiments", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)

dat %>% 
  filter(dat$Outcome == "AggCog" & dat$Setting == "Exp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Cognition, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggCog" & dat$Setting == "Exp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Cognition, All Experiments", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggCog" & dat$Setting == "Exp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)

dev.off()



png("Anderson_funnels_2.png", width = 5, height = 6, units = 'in', res = 280)
# prepare faceting
par(mfrow = c(3,2),
    mar = c(2, 4, 2, 1) + .01)

dat %>% 
  filter(dat$Outcome == "AggAff" & dat$Setting == "Nonexp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Affect, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggAff" & dat$Setting == "Nonexp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Affect, All Correlational", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggAff" & dat$Setting == "Nonexp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)


dat %>% 
  filter(dat$Outcome == "AggBeh" & dat$Setting == "Nonexp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Behavior, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggBeh" & dat$Setting == "Nonexp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Behavior, All Correlational", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggBeh" & dat$Setting == "Nonexp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)

dat %>% 
  filter(dat$Outcome == "AggCog" & dat$Setting == "Nonexp" & dat$Best. %in% c("y")) %>% 
  funnelPETPEESE(plotName = "Aggressive Cognition, Best-Practices", printText = F)

dat %>% 
  filter(dat$Outcome == "AggCog" & dat$Setting == "Nonexp" & dat$Best. %in% c("y", "n")) %>% 
  funnelPETPEESE(plotName = "Aggressive Cognition, All Correlational", printText = F, pch = ifelse(.$Best. == "y", 16, 1))
# dat %>% 
#   filter(dat$Outcome == "AggCog" & dat$Setting == "Nonexp" & dat$Best. %in% c("n")) %$% 
#   points(x = Fisher.s.Z, y = Std.Err, pch = 4, cex = 1.5)

dev.off()
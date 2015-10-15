# Anderson_2010 master script

source("Anderson_cleaning.R")
source("Anderson_2010_PET-PEESE.R")
source("Anderson_pcurve.R")
source("Anderson_dissertations.R")

# aggregate results ----
library(dplyr)
results1 = read.delim("PETPEESE_output.txt")
results2 = read.delim("pcurve_results.txt")
full_join(results2, results1) %>% 
  write.table(file = "full_results.txt", sep="\t", row.names=F)

# export plots in publication-ready format ----
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
source("PETPEESE_functions.R")
# Affect ----
windows(width = 12, height = 4)
par(mfrow = c(1, 3))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F)
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F)
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F)
savePlot(filename="funnels_AggAff.png", type="png")
dev.off()

# Behavior ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best cross-sectional", printText = F)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F)
savePlot(filename="funnels_AggBeh.png", type="png")
dev.off()

# Cognition ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best cross-sectional", printText = F)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F)
savePlot(filename="funnels_AggCog.png", type="png")
dev.off()

# Arousal ----
windows(width = 8, height = 4)
par(mfrow = c(1, 2))
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F)
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F)
savePlot(filename="funnels_PhysArous.png", type="png")
dev.off()

cat("And that's a wrap!")

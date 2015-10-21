dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

## Read in PET-PEESE functions
source("PETPEESE_functions.R")
# get dplyr package for distinct()
library(dplyr)

dat$CRTT = F
noiseExps = c("ACF04AB2", "AD00AB2", "AM03AB", "BA02ABF", "BA02ABM", "BA02ABF/BA02ABM")
dat$CRTT[dat$Study.name %in% noiseExps] = T

dat.aggBeh = 
  dat %>% 
  filter(Outcome == "AggBeh", Setting == "Exp")

model = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh)
model
funnel(model, pch = ifelse(dat.aggBeh$CRTT, 16, 1), cex = ifelse(dat.aggBeh$CRTT, 1.1, .5),
       level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline = 0,
       main = "Bias in CRTT?")
egger = rma(yi = Fisher.s.Z, sei = Std.Err, mods = ~Std.Err, 
            data = dat.aggBeh, subset = CRTT == T)
egger
b = egger$b
abline(a = -b[1]/b[2], b = 1/b[2])

crtt.1 = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh, subset = CRTT == T)
crtt.2 = rma(yi = Fisher.s.Z, sei = Std.Err, data = dat.aggBeh, subset = CRTT == F)
crtt.1
crtt.2

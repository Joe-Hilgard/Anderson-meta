# Trim-and-fill analyses
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

## Read in PET-PEESE functions
source("PETPEESE_functions.R")
# get dplyr package for distinct()
library(plyr)
library(dplyr)
library(metafor)
# Prepare export folder, just in case
dir.create("./supplement") 

# Big old loop
outputFrame = NULL

# Note that they originally lumped together the cross-sectional and experimental effects
# for their "Full" analyses. TAF on experiments were limited to the "best"
# Cross-sections were restricted to the partial effect sizes
# Sometimes split by East/West but sometimes not. Maddeningly inconsistent!!
for (k in c("East", "West")) {
  filter = dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. == "y" & dat$East.West == k
  mod = naive(dat[filter,])
  modTF = trimfill(mod)
  
  # Output data frame
  output = data.frame(
    # ID data
    "Outcome" = "AggBeh",
    "Setting" = "Exp",
    "East/West" = k,
    "Best" = "y",
    "K" = modTF$k,
    "K-imputed" = modTF$k0,
    "Direction" = modTF$side,
    "naive.r" = tanh(mod$b[1]),
    "TAF.r" = tanh(modTF$b[1]),
    "change.r" = tanh(modTF$b[1]) - tanh(mod$b[1]),
    "TAF.r.lb" = tanh(modTF$ci.lb[1]),
    "TAF.r.ub" = tanh(modTF$ci.ub[1])
  )
  outputFrame = rbind(outputFrame, output)
}

for (i in c("AggCog", "AggAff", "ProsBeh", "Empathy", "Desen", "PhysArous")) {
  for (j in c("Exp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    filter = dat$Outcome == i & dat$Setting == j & dat$Best. == "y"
    if(sum(filter) <= 1) next
    mod = naive(dat[filter,])
    modTF = trimfill(mod)
    
    # Output data frame
    output = data.frame(
      # ID data
      "Outcome" = i,
      "Setting" = j,
      "East/West" = "Both",
      "Best" = "y",
      "K" = modTF$k,
      "K-imputed" = modTF$k0,
      "Direction" = modTF$side,
      "naive.r" = tanh(mod$b[1]),
      "TAF.r" = tanh(modTF$b[1]),
      "change.r" = tanh(modTF$b[1]) - tanh(mod$b[1]),
      "TAF.r.lb" = tanh(modTF$ci.lb[1]),
      "TAF.r.ub" = tanh(modTF$ci.ub[1])
    )
    outputFrame = rbind(outputFrame, output)
  }
}
outputFrame[,sapply(outputFrame, is.numeric)] = round(outputFrame[,sapply(outputFrame, is.numeric)],3)

# May want to shave off some of the less-useful columns
write.table(outputFrame, "supplement/TAF_output.txt", row.names=F, sep="\t")

# Sensitivity: Prepare dataset with irrelevant Panee & Ballard effect
# Cleaning Anderson's dataset

library(plyr)
library(dplyr)
library(readxl)

source("table_managing_functions.R")
dat = read_excel("Master9-5-2short.xls")
# Bring in and append my coding information re: publication status
dat2 = read_excel("Pub_Style.xlsx")
dat = left_join(dat, dat2)
# Fix column names
names(dat) = c("ID", "Study.name", "Outcome", "PERSON", "Sample.size",
               "Correlation", "Fisher.s.Z", "Std.Err", "Full.Reference",
               "Long.Dup", "raw.r.", "Study", "SEX", "Best.", "AGE",
               "East.West", "setting2", "Setting", "1st", "2nd", "3rd", "4th",
               "aveORonlyR", "aveDVs", "Country", "Pub")
# Std.Err refers to Std.Err of z-transformed value
dat = dat[dat$Best. != "",] # delete the blank row
# Fix excess space in full reference of one entry.
dat$Full.Reference[dat$Full.Reference == "Austin, L. H. (1987). The effects of playing video games with aggressive features.Dissertation: The Fielding Institute, "] =
  "Austin, L. H. (1987). The effects of playing video games with aggressive features.Dissertation: The Fielding Institute,"

# Keep things disaggregated: drop the MF row when there's M and F separately 
# Note that these have setting "NonexpS." What a chore! Using column "setting2"?
# I don't know what suffixes "oc" and "ocs" and "oce" and "yc" and "ycs" and "ye" mean in this context. Ugh!
dat = dat[!(dat$Study.name %in% 
              c("AD00AB1b", # Anderson & Dill, 2000, s1
                "AGB07AA2", "AGB07AB2", "AGB07AC2", #AG&B 07 s2
                "AGB07AB3", "AGB07AC3", "AGB07PB3"  #AG&B 07 S3
              )),] # The rest of double-entries (for aggbeh at least) are separate M&F with no MF row.

# Generate t-values and p-values and cohen's d from Fisher's Z and SE(Z)
# One-tailed? Two-tailed?
dat$z = dat$Fisher.s.Z / dat$Std.Err
dat$t = dat$Correlation * sqrt((dat$Sample.size - 2) / (1-dat$Correlation^2))
dat$df = dat$Sample.size - 2
dat$d_est = 2*dat$Correlation/sqrt(1 - dat$Correlation^2) # per Borenstein textbook p. 48
dat$p.onetail = pt(dat$t, df = dat$df, lower.tail=F) 
dat$p.twotail = 2*pt(abs(dat$t), df = dat$df, lower.tail=F) 
# kludge for hypothesized decreases in empathy
dat$p.onetail[dat$Outcome == "Empathy"] = pt(dat$t[dat$Outcome == "Empathy"], 
                                             df = dat$df[dat$Outcome == "Empathy"], 
                                             lower.tail=T) 

outputFrame = NULL
for (k in c("East", "West")) {
  filter = dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. == "y" & dat$East.West == k
  mod = naive(dat[filter,])
  modTF = trimfill(mod)
  
  # Output data frame
  output = data.frame(
    # ID data
    "Outcome" = "AggBeh",
    "Setting" = "Exp",
    "East/West" = k,
    "Best" = "y",
    "K" = modTF$k,
    "K-imputed" = modTF$k0,
    "Direction" = modTF$side,
    "naive.r" = tanh(mod$b[1]),
    "TAF.r" = tanh(modTF$b[1]),
    "change.r" = tanh(modTF$b[1]) - tanh(mod$b[1]),
    "TAF.r.lb" = tanh(modTF$ci.lb[1]),
    "TAF.r.ub" = tanh(modTF$ci.ub[1])
  )
  outputFrame = rbind(outputFrame, output)
}

for (i in c("AggCog", "AggAff", "ProsBeh", "Empathy", "Desen", "PhysArous")) {
  for (j in c("Exp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    filter = dat$Outcome == i & dat$Setting == j & dat$Best. == "y"
    if(sum(filter) <= 1) next
    mod = naive(dat[filter,])
    modTF = trimfill(mod)
    
    # Output data frame
    output = data.frame(
      # ID data
      "Outcome" = i,
      "Setting" = j,
      "East/West" = "Both",
      "Best" = "y",
      "K" = modTF$k,
      "K-imputed" = modTF$k0,
      "Direction" = modTF$side,
      "naive.r" = tanh(mod$b[1]),
      "TAF.r" = tanh(modTF$b[1]),
      "change.r" = tanh(modTF$b[1]) - tanh(mod$b[1]),
      "TAF.r.lb" = tanh(modTF$ci.lb[1]),
      "TAF.r.ub" = tanh(modTF$ci.ub[1])
    )
    outputFrame = rbind(outputFrame, output)
  }
}
outputFrame[,sapply(outputFrame, is.numeric)] = round(outputFrame[,sapply(outputFrame, is.numeric)],3)

# May want to shave off some of the less-useful columns
write.table(outputFrame, "supplement/TAF_output_noexclusion.txt", row.names=F, sep="\t")

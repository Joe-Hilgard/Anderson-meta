# Cleaning Anderson's dataset

library(plyr)
library(readxl)
library(dplyr)

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


# Inspect suspicious Matsuzaki et al study
# View(dat[grep("Matsuzaki", dat$Full.Reference),])
# I'm going to remove Matsuzaki et al study 1 because it is an outlier in every analysis
dat = dat[!(dat$Study.name %in% c("MWS04ABb", "MWS04ABn", "MWS04AC")),]
# Drop Brady & Matthews (06) as it's the same as Brady (06) but divided into more rows
dat = dat[-grep("Brady, S.S., & Mathews", dat$Full.Reference),]
# Drop Panee & Ballard (2002) because in-game behavior is not aggressive behavior,
  # Nor is this a violent videogame manipulation (it is a priming manipulation)
dat = dat[-grep("Panee, C. D., & Ballard", dat$Full.Reference),]
# Drop Graybill et al. (1985) because manipulation check is not aggressive cognition
dat = dat[-grep("Graybill, D., Kirsch, J. R.", dat$Full.Reference),]

# I'm also gonna drop the redundant M,F rows when there's already an MF row. 
dat = dat[!(dat$Study.name %in% 
              c("AD00AB1bF", "AD00AB1bM" # Anderson & Dill, 2000, s1
                ,"AGB07AA2F", "AGB07AA2M", "AGB07AB2F", "AGB07AB2M", "AGB07AC2F", "AGB07AC2M" #AG&B 07 s2
                ,"AGB07AB3F", "AGB07AB3M", "AGB07AC3F", "AGB07AC3M", "AGB07PB3F", "AGB07PB3M" #AG&B 07 S3
              )),] # The rest of double-entries (for aggbeh at least) are separate M&F with no MF row.
for (i in unique(dat$Outcome)) {
  for (j in "Exp") { 
    for (k in 1:2) { # Anderson didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      print(name)
      print(dim(getTooMany(dat, filter))[1])
    }
  }
}
# Too many AggBeh Exp
getTooMany(dat, dat$Setting == "Exp" & dat$Outcome == "AggBeh" & dat$Best. == "y")
# aggregate young/old in AG&B 07 S2
dat = combine.rows(dat, dat$Study.name %in% c("AGB07AB1oe", "AGB07AB1ye"), "sum")
# aggregate men/women in B&A 02
dat = combine.rows(dat, dat$Study.name %in% c("BA02ABF", "BA02ABM"), "sum")

# get too many AggBeh not-best
getTooMany(dat, dat$Setting == "Exp" & dat$Outcome == "AggBeh" & dat$Best. == "n")
# Looks like older and younger, maybe a median split? Let's sum.
dat = combine.rows(dat, dat$Study.name %in% c("M03ABo", "M03ABy"), "sum")

# get too many AggCog not-best
getTooMany(dat, dat$Setting == "Exp" & dat$Outcome == "AggCog" & dat$Best. == "n")
# looks again like older/younger, male/female splits, so let's sum.
dat = combine.rows(dat, dat$Study.name %in% c("FBJ03ACFo", "FBJ03ACFy", "FBJ03ACMo", "FBJ03ACMy"), "sum")
dat = combine.rows(dat, dat$Study.name %in% c("M03ACo", "M03ACy"), "sum")
dat = combine.rows(dat, dat$Study.name %in% c("S95ACF", "S95ACM"), "sum")

# get too many PhysArous best
getTooMany(dat, dat$Setting == "Exp" & dat$Outcome == "PhysArous" & dat$Best. == "y")
# male/female split
dat = combine.rows(dat, dat$Study.name %in% c("AEC06PAF", "AEC06PAM"), "sum")

# get too many PhysArous not-best
getTooMany(dat, dat$Setting == "Exp" & dat$Outcome == "PhysArous" & dat$Best. == "n")
# male/female split
dat = combine.rows(dat, dat$Study.name %in% c("M07PAF", "M07PAM"), "sum")

# What about nonexp?
for (i in unique(dat$Outcome)) {
  for (j in "Nonexp") { 
    for (k in 1:2) { 
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two rows
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      print(name)
      print(dim(getTooMany(dat, filter))[1])
    }
  }
}

# Too many AggBeh best nonexp
getTooMany(dat, dat$Setting == "Nonexp" & dat$Outcome == "AggBeh" & dat$Best. == "y")
# combine older & younger in AGB07 S1
dat = combine.rows(dat, dat$Study.name %in% c("AGB07AB1oc", "AGB07AB1yc"), "sum")

# Too many AggCog best nonexp
getTooMany(dat, dat$Setting == "Nonexp" & dat$Outcome == "AggCog" & dat$Best. == "y")
# Hoph et al accidentally mislabeled "NonexpS" as "Nonexp"
dat = dat[dat$Study.name != "HHW08ACs",]

# And Long?
# What about nonexp?
for (i in unique(dat$Outcome)) {
  for (j in "Long") { 
    for (k in 1:2) { 
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      print(name)
      print(dim(getTooMany(dat, filter))[1])
    }
  }
}

# lots of too-many. ugh. 
  # let's avoid the longitudinal stuff b/c prob won't be enough datapoints for meta-reg
  # Also the distinctions between Long, LongS, LongP, LongPS make my head hurt.

# Could deal with Empathy and Desen some other time, maybe. Super small samples

# # Potential alternative model: Censoring of individual subsamples
# # Alternatively, drop the MF rows when M and F exist
# dat1 = dat1[!(dat$Study.name %in%
#                 c("AD00AB1b",
#                   "AGB07AA2", "AGB07AB2", "AGB07AC2",
#                   "AGB07AB3", "AGB07AC", "AGB07PB",
#                   "BA02AB")),]


# dump the use of sex as a control for now in dataset  
  # Later to be made separate dat1, maybe
dat = dat[dat$Setting %in% c("Exp", "Nonexp", "Long") 
          # Sometimes studies are entered with S suffix and "MF" when effect
            # was reported only once, including gender covariate
          # in case this is the only way the correlational study was reported:
          | (dat$Setting %in% "NonexpS" & dat$SEX %in% c("M", "F"))  
          # or for longitudinal studies
          | (dat$Setting %in% "LongPs" & dat$SEX %in% c("M", "F")) 
          ,]
# dat$Setting[dat$Setting == "NonexpS"] = "Nonexp" 
  # will need to combine male and female samples

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


write.table(dat, file="cleaned_data.txt", sep="\t", row.names=F)

# Cleaning Anderson's dataset

require(plyr)

setwd("~/GitHub/craig_meta")
source("table_managing_functions.R")
dat = read.delim("Craig_Table_2010.txt", stringsAsFactors=F)
dat = dat[dat$Best.!="",] # delete the blank row
dat = dat[!is.na(dat$Correlation),] # delete my dumb entries where I aggregated by hand
# Std.Err refers to Std.Err of z-transformed value

# Inspect suspicious Matsuzaki et al study
# View(dat[grep("Matsuzaki", dat$Full.Reference),])
# !! I'm going to remove Matsuzaki et al study 1 because it is an outlier in every analysis
dat = dat[!(dat$Study.name %in% c("MWS04ABb", "MWS04ABn", "MWS04AC")),]
# Drop Brady & Matthews (06) as it's the same as Brady (06) but divided into more rows
dat = dat[-grep("Brady, S.S., & Mathews", dat$Full.Reference),]
# I'm also gonna drop the redundant M,F rows when there's already an MF row. 
dat1 = dat
dat = dat[!(dat$Study.name %in% 
              c("AD00AB1bF", "AD00AB1bM" # Anderson & Dill, 2000, s1
                ,"AGB07AA2F", "AGB07AA2M", "AGB07AB2F", "AGB07AB2M", "AGB07AC2F", "AGB07AC2M" #AG&B 07 s2
                ,"AGB07AB3F", "AGB07AB3M", "AGB07AC3F", "AGB07AC3M", "AGB07PB3F", "AGB07PB3M" #AG&B 07 S3
              )),] # The rest of double-entries (for aggbeh at least) are separate M&F with no MF row.
for (i in unique(dat$Outcome)) {
  for (j in unique(dat$Setting)) {
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      filter = dat$Outcome == i & dat$Setting == j & dat$Best. %in% best[[k]]
      if (sum(filter) < 3) next # must have at least two studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Best?: ", k
                   , sep="")
      print(name)
      print(dim(getTooMany(dat[filter,]))[1])
    }
  }
}

# combining data


# # Potential alternative model: Censoring of individual subsamples
# # Alternatively, drop the MF rows when M and F exist
# dat1 = dat1[!(dat$Study.name %in%
#                 c("AD00AB1b",
#                   "AGB07AA2", "AGB07AB2", "AGB07AC2",
#                   "AGB07AB3", "AGB07AC", "AGB07PB",
#                   "BA02AB")),]


# dump the use of sex as a control for now in dataset  # Later to be made separate dat1
dat = dat[dat$Setting %in% c("Exp", "Nonexp", "Long") 
          # in case this is the only way the correlational study was reported:
          | (dat$Setting %in% "NonexpS" & dat$SEX %in% c("M", "F"))  
          # or for longitudinal studies
          #| (dat$Setting %in% "LongPs") 
          ,]
dat$Setting[dat$Setting == "NonexpS"] = "Nonexp"
#dat$Setting[dat$Setting %in% c("LongP", "LongPs"]

# what effect sizes within a study appear on more than one row?
# A lot of damn studies! Ass! 
# Some of it might be men & women entered separately (questionable for me)
# But others appear to be two DVs from same study (not acceptible, should be one line)
# Abstract me into a function, please!

View(getTooMany(dat, dat$Outcome == "AggBeh" & dat$Setting == "Exp" & dat$Best. == "y"))
# Most double-entries are due to splitting between Male/Female, however
# Carnagey & Anderson (2009) seems to routinely have 3 rows: M, F, and M&F.
# Need to handle that. Check the article and make sure the total N is just the one row.

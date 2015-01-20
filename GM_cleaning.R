# Cleaning script for G&M 2014
# Goal: One study is one row.

# TO DO:
# Can the code be refactored to remove the 5x loop?
# What to do with multiple pairwise? e.g. Krcmar; Valadez & Ferg
# Might just enter those by hand based on complex contrast.
# With odd n=11-looking sample from Anderson?

# Read in the data
setwd("~/GitHub/Craig_meta")
source("table_managing_functions.R")
dat=read.delim("GM2014_full_raw_data.txt", stringsAsFactors=F)
dat=dat[, names(dat)!=("X")]
table(dat$outcome.type, dat$Outcome.Group)
# delete the empty rows
dat = dat[!(dat$outcome.type == "" & dat$Outcome.Group == ""),]
# Create unique row identifier:
dat$ID = 1:dim(dat)[1]
# make very short names
dat$StudyShort = paste(substr(dat$Study, 1, 8), 
                       substr(dat$Study, nchar(dat$Study)-9, nchar(dat$Study)),
                       sep="_")
# Get apparent N given reported std.err of z
dat$Sample.size = (1/dat$Std.Err.1)^2+3
# rename standard error columns to match Anderson's
names(dat)[names(dat)=="Std.Err"] = "Std.Err.r"
names(dat)[names(dat)=="Std.Err.1"] = "Std.Err"

# Each outcome from each study should be just one point. 
# Then I'd like to break it up by Outcome for PETPEESE (outcome.group)
# Remember that all I've got is antisocial media, they didn't give me
#   the prosocial media stuff yet. (media.type)
# Shouldn't have to break it up by outcome.type. Well, then again...
#   behavior could be more aggressive and less prosocial...

select = getTooMany2(dat, list(dat$Study, dat$Outcome.Group))
#View(dat[dat$Study %in% select,]) # oh my god there's so much to handle
# exporting it for now for later Excel inspection
tooManyDat = dat[dat$Study %in% select,]
write.table(tooManyDat, file="GM_toomany.txt", row.names=F, sep="\t")

# As a general rule I'll just need to combine.rows("average")
  # across entries within dat$Outcome..specific.
# And combine.rows("sum")
  # across entries within dat$Subgroup.within.study
# But there are exceptions:
  # dat$Outcome..specific.:
    # ??? No exceptions ???
  # dat$Subgroup.within.study
    # Anderson & Carnagey (2009) appears twice for same subs, different DV
      # but one is entered as 103 subs and the other as 11 subs
    # pairwise comparisons may double- or triple-count the comparison group
      # e.g. Kcrmar & Lachlan (2009) or Valadez & Ferguson (2010)
backup = dat
# Collapse across measures within subgroups within outcomes within studies
for (i in unique(tooManyDat$Study)) {
  filterStudy = dat$Study == i
  print(i); print(length(filterStudy))
  for (j in unique(dat$Outcome.Group[filterStudy])) {
    filterOutcome = dat$Outcome.Group == j
    print(j); print(length(filterOutcome))
    for (k in unique(dat$Subgroup.within.study[filterStudy & filterOutcome])) {
      for (l in unique(dat$study.design)) {
        for (m in unique(dat$outcome.type)) {
      filter = dat$Study == i & dat$Outcome.Group == j & dat$Subgroup.within.study == k &
        dat$study.design == l & dat$outcome.type == m
      if (sum(filter) < 2) next
      #print(paste("Study:", i,
      #            "Outcome:", j,
      #            "Subgroup:", k))
      dat = combine.rows(dat, filter, "average")
      # Update filter for next iteration through this loop,
      # length has shrunk by at least 1!
      filterStudy = dat$Study==i
      filterOutcome = dat$Outcome.Group==j
    }
  }
}
}
}
# export for inspection
write.table(dat, "GM_2014_averaged.txt", sep="\t", row.names=F)

# Now time to add subgroups
# get the tooManyDat again
select = getTooMany2(dat, list(dat$Study, dat$Outcome.Group))
tooManyDat = dat[dat$Study %in% select,]

backup = dat

# Collapse across subgroups within outcomes within studies
for (i in unique(tooManyDat$Study)) {
  filterStudy = dat$Study == i
  print(i); print(length(filterStudy))
  for (j in unique(dat$Outcome.Group[filterStudy])) {
    filterOutcome = dat$Outcome.Group == j
    print(j); print(length(filterOutcome))
    for (k in unique(dat$study.design)) {
      for (l in unique(dat$outcome.type)) {
    filter = dat$Study == i & dat$Outcome.Group == j &
      dat$study.design == k & dat$outcome.type == l
    if (sum(filter) < 2) next
      #print(paste("Study:", i,
      #            "Outcome:", j,
      #            "Subgroup:", k))
    dat = combine.rows(dat, filter, "sum")
      # Update filter for next iteration through this loop,
      # length has shrunk by at least 1!
    filterStudy = dat$Study==i
    filterOutcome = dat$Outcome.Group==j
  }
}
}
}
write.table(dat, "GM_2014_averaged_summed.txt", sep="\t", row.names=F)

# Cleaning script for G&M 2014
# Goal: One study is one row.

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
dat$N = (1/dat$Std.Err.1)^2+3

# Each outcome from each study should be just one point. 
# Then I'd like to break it up by Outcome for PETPEESE (outcome.group)
# Remember that all I've got is antisocial media, they didn't give me
#   the prosocial media stuff yet. (media.type)
# Shouldn't have to break it up by outcome.type. Well, then again...
#   behavior could be more aggressive and less prosocial...

select = getTooMany2(dat, list(dat$Study, dat$Outcome.Group))
View(dat[dat$Study %in% select,]) # oh my god there's so much to handle
# exporting it for now for later Excel inspection
write.table(dat[dat$Study %in% select,], file="GM_toomany.txt", row.names=F, sep="\t")

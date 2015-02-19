# Seems I've only got the violent-media stuff so I can't reproduce the
  # prosocial-media stuff. Could always ask...

# Need to determine how G&M sliced up these studies as reported in Tables 1 and 2...
# It's such a damn chore that several lines are reported for a single study
# I could maybe start by just filtering by Study Design
#   And then maybe aggregate (weighted mean) within studies with same name & outcome 
# I'd like to double-check all the Krcmar stuff with the millions of simple slopes

# I need to reproduce CMA's imputation/aggregation/filtering
  # For subgroups within studies, sum N and weighted average effect size
  # For outcomes within studies, weighted average N and effect size 

## Create functions 
source("PETPEESE_functions.R")
## read data
dat = read.delim("GM_2014_averaged_summed_fixWIP.txt", stringsAsFactors=F)
list(unique(dat$Outcome.Group), unique(dat$outcome.type), unique(dat$study.design)
     , unique(dat$media.type))
table(dat$Outcome.Group, dat$outcome.type, dat$study.design)
# Look for studies featuring a particular author
# View(dat2[grep("Anderson", dat$Study),])

# Make storage directories for output:
dir.create("./GM_petpeese_plotdump"); dir.create("./GM_petpeese_plotdump/diagnostics")

# Let's do this shit
for (i in unique(dat$Outcome.Group)) {
  for (j in unique(dat$study.design)) {
    for (k in unique(dat$outcome.type)) {
      #print(paste(i,j,k))
      filter = dat$Outcome.Group == i & dat$study.design == j & dat$outcome.type == k
      if (sum(filter) < 6) next # must have at least six studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Type: ", k
                   , sep="")
      windows()
      saveName = paste("./GM_petpeese_plotdump/", paste(i,j,substr(k,1,4), sep="_"),".png", sep="")
      print(name)
      funnelPET.RMA(dat[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}
# Let's get influence diagnostics
for (i in unique(dat$Outcome.Group)) {
  for (j in unique(dat$study.design)) {
    for (k in unique(dat$outcome.type)) {
      #print(paste(i,j,k))
      filter = dat$Outcome.Group == i & dat$study.design == j & dat$outcome.type == k
      if (sum(filter) < 6) next # must have at least six studies
      name = paste("Outcome: ", i,
                   ", Setting: ", j,
                   ", Type: ", k
                   , sep="")
      windows()
      saveName = paste("./GM_petpeese_plotdump/diagnostics/", paste(i,j,substr(k,1,4), sep="_"),".png", sep="")
      print(name)
      par(mfrow=c(2,2))
      leveragePET(dat[filter,], plotName = name)
      savePlot(filename=saveName, type="png")
      graphics.off()
    }
  }
}

# do it their way, slopping together longitudinal, correlational, and experimental
# I think this is a terrible idea but maybe I can reproduce their stats

# Make storage directories for output:
dir.create("./GM_munge1"); dir.create("./GM_munge2")

# Let's do this shit
for (i in unique(dat$Outcome.Group)) {
  # slop together long, cor, exp
  for (k in unique(dat$outcome.type)) {
    #print(paste(i,j,k))
    filter = dat$Outcome.Group == i & dat$outcome.type == k
    #if (sum(filter) < 6) next # I want RMA output even if PETPEESE no good
    name = paste("Outcome: ", i,
                 ", Type: ", k
                 , sep="")
    windows()
    saveName = paste("./GM_munge1/", paste(i,substr(k,1,4), sep="_"),".png", sep="")
    print(name)
    funnelPET.RMA(dat[filter,], plotName = name)
    savePlot(filename=saveName, type="png")
    graphics.off()
  }
}

# now collapse over outcome.type
#slop together aff,cog,beh,etc. #for (i in unique(dat$Outcome.Group)) {
for (j in unique(dat$study.design)) {
  for (k in unique(dat$outcome.type)) {
    #print(paste(i,j,k))
    filter = dat$study.design == j & dat$outcome.type==k
    if (sum(filter) < 3) next # I want RMA output even if PETPEESE no good
    name = paste("Design: ", j,
                 ", Type: ", k
                 , sep="")
    windows()
    saveName = paste("./GM_munge2/", paste(j,substr(k,1,4), sep="_"),".png", sep="")
    print(name)
    funnelPET.RMA(dat[filter,], plotName = name)
    savePlot(filename=saveName, type="png")
    graphics.off()
  }
}
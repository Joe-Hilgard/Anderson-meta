source("table_managing_functions.R")

dat = read.delim("GM_2014_averaged_summed.txt", stringsAsFactors=F)
dat.old = read.delim("GM2014_full_raw_data.txt", stringsAsFactors=F)
# Fix dat.old formatting
names(dat.old)[c(1,8,10)] = c("ID", "Std.Err.r", "Std.Err")
dat.old = dat.old[,c(2:16, 1)]
dat.old$StudyShort = NA; dat.old$Sample.size = NA 

# Issues to fix:

# Longitudinal study erroneously entered as experimental
  # ID: 10, von Salisch, M., Vogelgesang, J., Kristen, A., & Oppl, C. (2011).
dat$study.design[dat$ID == 10] = "experimental"

# Complex contrasts counted control group 2x, 3x, even 4x.
  # ID: 83/84/86/87/89/90/92/93, Krcmar, M. & Lachlan, K. (2009).
## Replacing with complex contrasts (ctrl vs. all others)
weights.KL = c(-1, rep(1/4, 4))
ns.KL = c(21, 36, 38, 41, 37)
physAgg.sd.KL = c(.98, 1.08, 1.35, 1.18, 1.05)
verbAgg.sd.KL = c(1.46, 1.52, 1.44, 1.41, 1.35)
arousal.sd.KL = c(.69, .92, 1.19, 1.23, 1.22)
physAgg.means.KL = c(1.56, 2.16, 2.72, 2.06, 1.84)
verbAgg.means.KL = c(1.75, 2.74, 2.74, 2.44, 2.20)
arousal.means.KL = c(2.95, 4.97, 4.59, 3.97, 3.55)
# get effect sizes d
physAgg.d.KL = contrast.d(weights=weights.KL,
                          means = physAgg.means.KL,
                          sds = physAgg.sd.KL,                          
                          ns = ns.KL
)
verbAgg.d.KL = contrast.d(weights=weights.KL,
                          means = verbAgg.means.KL,
                          sds = verbAgg.sd.KL,                          
                          ns = ns.KL
)
arousal.d.KL = contrast.d(weights=weights.KL,
                          means = arousal.means.KL,
                          sds = arousal.sd.KL,                          
                          ns = ns.KL
)
# convert d to Fisher's z
physAgg.z = d2r2z(physAgg.d.KL, ns.KL[1], sum(ns.KL[2:5]))
verbAgg.z = d2r2z(verbAgg.d.KL, ns.KL[1], sum(ns.KL[2:5]))
arousal.z = d2r2z(arousal.d.KL, ns.KL[1], sum(ns.KL[2:5]))
# Now aggregate phys and verb aggression
agg.z = weighted.mean(x = c(physAgg.z$Z, verbAgg.z$Z),
                      w = c(1/physAgg.z$StdErr.z, 1/verbAgg.z$StdErr.z)
)
agg.StdErr = mean(c(physAgg.z$StdErr.z, verbAgg.z$StdErr.z))
# Enter correct values
# aggression:
dat$Fisher.s.Z[dat$ID %in% "83/84/86/87/89/90/92/93"] = agg.z
dat$Std.Err[dat$ID %in% "83/84/86/87/89/90/92/93"] = agg.StdErr
# arousal:
dat$Fisher.s.Z[dat$ID %in% "82/85/88/91"] = arousal.z$Z
dat$Fisher.s.Z[dat$ID %in% "82/85/88/91"] = arousal.z$StdErr.z
  # ID: 225/226/227, Ewoldsen, D. R. et al. (2011)

  # ID: 11/12/13/14/15/16, Valadez, J. J., & Ferguson, C. J. (2012).

# Sestir and Bartholow (2010) subsample given precision of full group
  # ID: 24, Sestir, M. A., & Bartholow, B. D. (2010).
dat$Std.Err[dat$ID == 24] = 1/sqrt((347/3)-3) #THIS IS AN APPROXIMATION
# I DON'T KNOW THE ACTUAL SAMPLE SIZE AND THIS EQUATION IS A ROUGH APPROXIMATION
# Total sample N was 347 and this is 1/3rd of that (discarding 15-min and ctrl group)
# I might suggest alternatively including the entire sample

# Raising an eyebrow at Barlett subgroup "45 min (T4)"
  # This suggests that there might be other effect sizes that should've been
    # aggregated together

# "Ferguson" code column errors:
  # ID: 231/232, Engelhardt, C. R., Bartholow, B. D., & Saults, S. (2011). coded as 2
    # But no anderson/bushman co-author (not directly at least)
dat$Ferguson[dat$ID == "231/232"] = 3
  # ID: 119, Jerabeck, J. M., Ferguson, C. J. (2013). coded as 2
    # Should be 1, these are Ferguson, not Anderson/Bushman
dat$Ferguson[dat$ID == 119] = 1

# ID: 260/261, Anderson & Carnagey S3 treated dependent measures as independent
  # "High intensity aggression" and "average intensity aggression"
  # And one of these was given very wide standard error
  # The two mistakes may cancel each other out but should still be fixed
meanZ = mean(dat.old$Fisher.s.Z[dat.old$ID %in% c(260, 261)])
dat$Fisher.s.Z[dat$ID == "260/261"] = meanZ
# Can't check what the SE should be right now but can set it to that of avg. int
dat$Std.Err[dat$ID == "260/261"] = dat.old$Std.Err[dat.old$ID %in% 260]

# Inappropriate effect sizes
  # ID: 125/126, Hasan, Y., Bègue, L., & Bushman, B. J. (2012). 
    # "Hostile Expectation Bias" mediator entered as aggressive behavior
dat = dat[dat$ID != "125/126",] # drop the aggregated row
# Bring back in the unaggregated rows
dat = rbind(dat, dat.old[dat.old$ID %in% c(125, 126),]) 
# Fix "hostile expectation bias" to be cognition, if anything. 
# Maybe delete, depending on your definition of agg cog & implied intent
dat$Outcome.Group[dat$ID == 126] = "cognition"





write.table(dat, file="GM_2014_averaged_summed_fixed.txt", sep="\t", row.names=F)


#####
# Ewoldsen


## Jerabeck and Ferguson complex contrast
# N = 100 divided somehow evenly across six cells... 16x4 + 18x4?
#  dependency within dyads not modeled.
# Drops the violent-prosocial condition, so no multiple comparisons here.
# "In fact, the means for antisocial video games were
# least aggressive (M = 1.87, SD = 0.61) compared to either the prosocial
# violent (M = 2.15, SD = 0.89) or non-violent (M = 1.91, SD = 0.90) games. 
# Neither covariate was statistically significant,
# suggesting confounds were not at issue."
## Krcmar & Lachlan complex contrast
# Single complex contrast
ns = c(21, 36, 38, 41, 37)
physAgg.sd = pool.sd(sds=c(.98, 1.08, 1.35, 1.18, 1.05),
                     ns=ns)
verbAgg.sd = pool.sd(sds=c(1.46, 1.52, 1.44, 1.41, 1.35),
                     ns = ns)
arousal.sd = pool.sd(sds=c(.69, .92, 1.19, 1.23, 1.22),
                     ns = ns)

# effect size d
contrast = c(-1, rep(1/4, 4))
numerator = sum(ns * c(1.56, 2.16, 2.72, 2.06, 1.84) * contrast) / sum(ns * abs(contrast))
physAgg.d = numerator / physAgg.sd

numerator = sum(ns * c(1.75, 2.74, 2.74, 2.44, 2.20) * contrast) / sum(ns * abs(contrast))
verbAgg.d = numerator / verbAgg.sd

numerator = sum(ns * c(2.95, 4.97, 4.59, 3.97, 3.55) * contrast) / sum(ns * abs(contrast))
arousal.d = numerator / arousal.sd

# to Pearson r
physAgg.r = d2r(physAgg.d, ns[1], sum(ns[2:5]))
verbAgg.r = d2r(verbAgg.d, ns[1], sum(ns[2:5]))
arousal.r = d2r(arousal.d, ns[1], sum(ns[2:5]))

# to Fisher's z
(physAgg.z = atanh(physAgg.r$r))
(verbAgg.z = atanh(verbAgg.r$r))
(arousal.z = atanh(arousal.r$r))

# one standard error to rule them all
1/sqrt(sum(ns)-3)

# Anderson & Carnagey (2009)
# two outcomes erroneously as subgroups
# One had very poor precision, commensurate with N=11
# Calculate average effect by hand.

## Jerabeck and Ferguson complex contrast
# N = 100 divided somehow evenly across six cells... 16x4 + 18x4?
#  dependency within dyads not modeled.
# Drops the violent-prosocial condition, so no multiple comparisons here.
# In fact, the means for antisocial video games were
# least aggressive (M = 1.87, SD = 0.61) compared to either the prosocial
# violent (M = 2.15, SD = 0.89) or non-violent (M = 1.91,
#                                               SD = 0.90) games. Neither covariate was statistically significant,
# suggesting confounds were not at issue.
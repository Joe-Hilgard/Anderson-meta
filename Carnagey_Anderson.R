# Carnagey & Anderson 2005 re-extraction of effect sizes
# Study 1: Agg Affect
means = c(2.52, 2.38, 2.02)
sds = c(.74, .51, .39)
ns = c(25, 25, 25)
s = pool.sd(sds, ns)
d = (mean(means[1:2]) - mean(means[3]))/s
d2r(d, sum(ns[1:2]), sum(ns[3]))
# This is a little less than what Craig entered. Can I reproduce his r=.39?
s = pool.sd(sds[c(1,3)], ns[c(1,3)])
d = (means[1]-means[3])/s
d2r(d, ns[1], ns[3]) # yep, excluding punish group

# Study 2: Agg Cog
means = c(.210, .175, .157)
sds = c(.066, .046, .050)
ns = rep(66/3, 3)
s = pool.sd(sds, ns)
d = (mean(means[1:2]) - mean(means[3]))/s
d2r(d, sum(ns[1:2]), sum(ns[3]))
# This is less than what Craig entered. Can I reproduce his r=.39?
s = pool.sd(sds[c(1,3)], ns[c(1,3)])
d = (means[1]-means[3])/s
d2r(d, ns[1], ns[3]) # this is too big

# Study 3: Agg Beh
means = c(177.0, 125.0, 116.9)
sds = c(167.58, 88.32, 74.67)
ns = rep(141/3, 3)
s = pool.sd(sds, ns)
d = (mean(means[1:2]) - mean(means[3]))/s
d2r(d, sum(ns[1:2]), sum(ns[3]))
# This is much less than what Craig entered. Can I reproduce his r=.39?
s = pool.sd(sds[c(1,3)], ns[c(1,3)])
d = (means[1]-means[3])/s
d2r(d, ns[1], ns[3]) # this is a little too big

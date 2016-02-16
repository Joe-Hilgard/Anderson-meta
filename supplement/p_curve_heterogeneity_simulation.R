# Simulation to examine behavior of pcurve under heterogeneity
# It's my impression that when the random-effects estimate is zero,
# but there's heterogeneity and one-tailed testing,
# the heterogeneous effect sizes will be overrepresented in p-curve
# due to its examination of only statistically-significant effect sizes.

# In that regard, maybe p-uniform would do better.

# I'm too stupid to calculate effect sizes and their variances right now
# So come back and do this later.

library(metafor)
library(ggplot2)
library(dplyr)
library(truncdist)

# simulate studies under fixed-effect assumption ----
k = 600 # number of studies
# get sample sizes
n = rtrunc(k, "pois", a = 20, b = 600, lambda = 40)
n1 = n/2
# set effect size
meanD = 0
# make set of studies
set.seed(42069)
t = rt(n = k, df = n - 2, ncp = sqrt(n1/2)*meanD)
d = 2*t/sqrt(2*n1)	
df = n - 2
d_v = ( (2*n1 / (n1^2)) + d^2 / (2*df) ) * (2*n1 / df)
d_se = sqrt(d_v)
p = pt(t, df = n - 2, lower.tail = F)
# combine
d0tau0 = data.frame(n, t, d, df, d_v, d_se, p, meanD)
# inspect p-curve
d0tau0 %>% 
  filter(p < .05) %>% 
  ggplot(aes(x = p, fill = as.factor(meanD))) +
  geom_bar(binwidth = .01)

# funnel plot
ggplot(d0tau0, aes(x = d, y = d_se, col = as.factor(meanD))) +
  geom_point(size = 3, alpha = .5) +
  scale_y_reverse() +
  geom_segment(aes(x = 0, xend = max(d_se)*1.98,
                   y = 0, yend = max(d_se))) +
  geom_segment(aes(x = 0, xend = -max(d_se)*1.98,
                   y = 0, yend = max(d_se)))

# simulate studies under random-effect assumption ----
k = 600 # please make this divisible by 3
# get sample sizes
n = rtrunc(k, "norm", a = 20, b = 600, mean = 40, sd = 20)
n1 = n/2
# set effect sizes
meanD = rep(c(0, .5, 1), each = k/3)
# make set of studies
set.seed(42069)
t = rt(n = 600, df = n - 2, ncp = sqrt(n1/2)*meanD)
d = 2*t/sqrt(2*n1)	
df = n - 2
d_v = ( (2*n1 / (n1^2)) + d^2 / (2*df) ) * (2*n1 / df)
d_se = sqrt(d_v)
p = pt(t, df = n - 2, lower.tail = F)
pcrit = qt(.975, df, 0)
d0tau1 = data.frame(n, t, d, df, d_v, d_se, p, pcrit, meanD)

# inspect p-curve ----
d0tau1 %>% 
  filter(p < .05) %>% 
  ggplot(aes(x = p, fill = as.factor(meanD))) +
  geom_bar(binwidth = .01)

# inspect funnel plot ----
mod = rma(yi = d, sei = d_se, data = d0tau1)
summary(mod)

d0tau1$meanD.color = factor(meanD, levels = c(0, .5, 1), 
                            labels = c("red", "green", "blue"))
d0tau1$meanD.pch = rep(c(15, 6, 11), each = 200)
funnel(mod, pch = d0tau1$meanD.pch)

ggplot(d0tau1, aes(x = d, y = d_se, col = as.factor(meanD))) +
  geom_point(size = 3, alpha = .5) +
  scale_y_reverse() +
  geom_segment(aes(x = 0, xend = max(d_se)*1.98,
                   y = 0, yend = max(d_se))) +
  geom_segment(aes(x = 0, xend = -max(d_se)*1.98,
                   y = 0, yend = max(d_se)))



# compare p-curve estimate against RMA estimate ----
source("Simonsohn-ESCI.R")
pcurve = plotloss(d0tau1$t, d0tau1$df, dmin=-.5, dmax = 3)
pcurve[1] # p-curve estimate in d
summary(mod)$b # RMA estimate in d
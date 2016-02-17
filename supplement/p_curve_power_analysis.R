# power analysis hypothesis:
# Reviewer 3 says:
# if the size of studies is chosen according to properly 
# executed power analyses, we would in fact expect to see 
# an inverse relationship between outcomes and sample sizes 
# (and so if authors engage in the recommended practice of 
# planning a study to achieve sufficient power, we are actually 
# building small-study effects into our literature!). 

# Let's simulate b/c I don't believe p-curve would work that way.

library(pwr)
library(metafor)

# lookup table for power
d = seq(.1, .6, .01) #seq(.1, 1, .05)
n = NULL
for (i in 1:length(d)) {
  n[i] = pwr.t.test(d = d[i], sig.level = .05, power = .8, 
                    type = "two.sample", alternative = "greater")$n
}
# round up b/c can't have fractional n
n = ceiling(n)

# pick a d, pick an n, run an experiment
simLength = 1e3
d_iter = NULL
n_iter = NULL
df_iter = NULL
t_iter = NULL
for (i in 1:simLength) {
  index = sample(1:length(d), 1)
  d_iter[i] = d[index]
  n_iter[i] = n[index]
  df_iter[i] = n_iter[i] - 2
  t_iter[i] = rt(1, df_iter[i], 
                 ncp = d_iter[i] / (sqrt(1/floor(n_iter[i]) + 1/ceiling(n_iter[i])))
  )
}

dat = data.frame(d_true = d_iter,
                 n = n_iter, df = df_iter, t = t_iter)
dat$d_obs = 2*dat$t/sqrt(dat$df)
dat$p = pt(dat$t, dat$df, lower.tail = F)
dat$se_obs = sqrt(
  (dat$n/((dat$n/2)^2)+dat$d_obs^2/(2*dat$df))*(dat$n/dat$df)
)

# funnel plot
model = rma(yi = d_obs, sei = se_obs, data = dat)

#p-curve
hist(dat$p)

par(mfrow=c(1, 2))
funnel(model, main = "Funnel plot w/ \npower analysis & \nheterogeneity")
hist(dat$p[dat$p<.05], main = "p-curve w/ \npower analysis & \nheterogeneity", xlab = "p-value")
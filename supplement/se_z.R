library(ggplot2)
library(dplyr)
library(tidyr)

# Let's inspect the magnitude of difference between formulae given effect size and allocation ratio
d = c(0, .25, .5)
ratio = c(.5, .66, .75)
n = c(40, 80, 120, 160, 200)
# Make data frame
dat = expand.grid(d, ratio, n)
names(dat) = c("delta", "ratio", "n")
# assign to groups
dat$n1 = floor(dat$n*dat$ratio)
dat$n2 = ceiling(dat$n*(1-dat$ratio))
# estimate r using proper and improper values of w^2 / a
dat$a = (dat$n1 + dat$n2)^2 / (dat$n1 * dat$n2)
dat$r.proper = dat$d / sqrt(dat$d^2 + dat$a)
dat$r.improper = dat$d / sqrt(dat$d^2 + 4)
# estimate Z
dat$z.proper = atanh(dat$r.proper)
dat$z.improper = atanh(dat$r.improper)
# estimate SE(Z)
# From Pustejovsky (2014), equation 16, letting w^2 = a
term1.proper = (1/(dat$d^2 + dat$a))
term1.improper = (1/(dat$d^2 + 4))
term2 = (dat$n1 + dat$n2)/(dat$n1*dat$n2) + dat$d^2/(2*(dat$n1+dat$n2))
dat$se.z_proper =  sqrt(term1.proper * term2)
dat$se.z_improper = sqrt(term1.improper * term2)
dat$se.z_approx = 1/sqrt(dat$n - 3)

# Empirical estimator
M = 1e4

goSimDX = function(n1, n2, es)
{
  d=1:M
  for (m in 1:M)
  {
    y1=rnorm(n1,0,1)
    y2=rnorm(n2,es,1)
    s=sqrt((var(y1)+var(y2))/2)
    d[m]=(mean(y2)-mean(y1))/s
  }
  r = d / sqrt(d ^ 2 + 4)
  z = 0.5 * log((1 + r) / (1 - r))
  return(c(mean(z),sd(z)))
}

for (i in 1:nrow(dat)) {
  temp = goSimDX(dat$n1[i], dat$n2[i], dat$delta[i])
  dat$z_empirical[i] = temp[1]
  dat$se.z_empirical[i] = temp[2]
}

# compare estimates
output = dat %>% 
  gather(se.z_proper:se.z_approx, se.z_empirical, key = "key", value = "SE_Z") %>% 
  separate(key, into = c("Stat", "Category"), sep = "_") %>% 
  filter(Category != "improper") 

output$delta.label = factor(output$delta, 
                         levels=c(0, .25, .5), 
                         labels = c("delta = 0", "delta = 0.25", "delta = 0.5"))
output$Estimator = factor(output$Category,
                               levels = c("approx", "proper", "empirical"),
                               labels = c("1/sqrt(N-3)", "Pustej. eq'n 16", "Empirical"))
output$Allocation.ratio = factor(output$ratio)
ggplot(output, aes(x = as.factor(n), y = SE_Z, col = Estimator, shape = Allocation.ratio)) +
  geom_point(size = 2, position = position_jitter(width = .40)) +
  facet_wrap(~delta.label) +
  scale_y_continuous("Standard Error of Z") +
  scale_x_discrete("Total sample size (jitter added for visibility)")

summary(dat$se.z_proper - dat$se.z_approx) # difference does not exceed .009 in SE
summary(dat$se.z_empirical - dat$se.z_approx) # difference does not exceed .005
# Even in that most extreme case, the difference is about the same as N = 17 vs. N = 18.
1/sqrt(18-3) - 1/sqrt(18-2) 

ggsave("./supplement/SE_Z_comparison.png", width = 5.8, height = 4.5, units = "in")




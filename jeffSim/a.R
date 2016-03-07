
M=10000


goSim=function(N,es)
{
  d=1:M
  for (m in 1:M)
  {
    y1=rnorm(N,0,1)
    y2=rnorm(N,es,1)
    s=sqrt((var(y1)+var(y2))/2)
    d[m]=(mean(y2)-mean(y1))/s
  }
  r = d / sqrt(d ^ 2 + 4)
  z=0.5 * log((1 + r) / (1 - r))
  return(c(mean(z),sd(z)))
}

myN=c(30,40,50,100,200,500)
J=length(myN)
out=matrix(ncol=2,nrow=J)
for (j in 1:J) out[j,]=goSim(myN[j],0)
out.2=matrix(ncol=2,nrow=J)
for (j in 1:J) out.2[j,]=goSim(myN[j],.2)
out.5=matrix(ncol=2,nrow=J)
for (j in 1:J) out.5[j,]=goSim(myN[j],.5)

pdf('sdCheck.pdf')
tform=sqrt(1/(2*myN-3))
plot(tform,out[,2],pch=19,col=rgb(1,0,0,.5),xlab="Standard Deviation of z from Borenstein Approximation",ylab="Standard Deviation of z (from simulation)")
points(tform,out.2[,2],pch=19,col=rgb(0,1,0,.5))
points(tform,out.5[,2],pch=19,col=rgb(0,0,1,.5))
legend("topleft",legend=c("True ES=0","True ES=.2","True ES=.5"),pch=19,col=c(rgb(1,0,0,.5),rgb(0,1,0,.5),rgb(0,0,1,.5)))
abline(0,1)
axis(3,at=tform,label=2*myN)
dev.off()

# Joe inspects magnitude of difference between formulae given effect size and allocation ratio
d = c(0, .25, .5)
ratio = c(.5, .66, .75)
n = c(40, 80, 120, 160, 200, 240, 280, 320)
# Make data frame
dat = expand.grid(d, ratio, n)
names(dat) = c("delta", "ratio", "n")
# assign to groups
dat$n1 = floor(dat$n*dat$ratio)
dat$n2 = ceiling(dat$n*(1-dat$ratio))
# estimate r using proper and improper values of w^2 / a
dat$a = (dat$n1 + dat$n2)^2 / (dat$n1 + dat$n2)
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
dat$se.z.proper =  term1.proper * term2
dat$se.z.improper = term1.improper * term2
dat$se.z.approx = 1/sqrt(dat$n - 3)

# compare estimates
library(ggplot2)

ggplot(dat, aes(x = se.z.proper, y = se.z.improper, 
                col = as.factor(ratio), shape = as.factor(delta))) +
  geom_point()


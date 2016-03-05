
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



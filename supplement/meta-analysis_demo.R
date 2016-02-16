#Script based on Carter & McCullough (2014) doi: 10.3389/fpsyg.2014.00823

#Load Libraries
library(meta)
library(metafor)

#Insert effect sizes and sample sizes
es.d<-c(0.38,0.41,-0.14,0.63,0.22)
n1<-c(75,48,22,18,60)
n2<-c(75,52,21,20,55)

#Calculate Variance ES
es.d.v <-(((n1+n2)/(n1*n2))+(es.d^2/(2*(n1+n2))))
#Calculate Standard Errors ES
d.se<-sqrt(es.d.v)

dat=read.delim("Craig_Table_2010.txt")
best = dat[dat$Outcome == "AggBeh" & dat$Long.Dup == "Exp",]
es.r = best$Cor.Joe
r.se = best$Std.Err
r.v  = r.se^2

metaset = data.frame(es.r, r.se, r.v)
metaset = metaset[order(metaset$r.se),]


#Fixed-effect and Random-effects meta-analysis
#Once with meta package, once with metafor package
rm("es.r", "r.se", "r.v"); 
attach(metaset)
meta1<-metagen(es.r, r.se)
meta2<-rma(es.r, r.v)

#Show results from both packages
meta1
meta2

#Forest Plot
#If you add studies, make sure to match number of labels in studlab variable.
forest(meta1, studlab=1:nrow(best), xlab="Pearson r", col.square="black",xlim=c(-1,1), col.diamond="black", fontsize=14, plotwidth=unit(12, "cm"), squaresize=0.5, leftcols=c("studlab"), rightcols=c("effect", "ci"), hetstat=FALSE, comb.fixed=FALSE, text.random="Overall ES", print.tau2=FALSE,print.I2=FALSE,TE.random=FALSE, seTE.random=FALSE) 

#Outlier Analysis and Check for Influential Cases
influence(meta2)
plot(influence(meta2))
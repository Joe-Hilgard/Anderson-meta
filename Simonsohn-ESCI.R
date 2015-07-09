# R-CODE for estimating effect size via p-curve - written by Uri Simonsohn 
# d-to-r conversion
d2r = function(d) {
  r = d / sqrt(d ^ 2 + 4)
}
#Define the loss function
loss=function(t,df,d_est) { #Syntax t: vector of t-values, df of degrees of freedom, d_est: candidate d
  t=abs(t) #Take absolute value of t-value (p-curve assumes same sign and/or sign does not matter)
  p_obs=2*(1-pt(t,df=df)) #Compute p-values of each t in t so as to keep only p<.05 results
  t.sig=subset(t,p_obs<.05) #Significant t-values
  df.sig=subset(df,p_obs<.05) #d.f. associated with significant t.values
  ncp_est=sqrt((df.sig+2)/4)*d_est #Compute noncentrality parameter for that sample size and candidate effect size
  tc=qt(.975,df.sig) #Compute critical t-value to get p=.05
  power_est=1-pt(tc,df.sig,ncp_est) #Compute power for obtaining that t-value or bigger, given the noncentrality parameter
  p_larger=pt(t.sig,df=df.sig,ncp=ncp_est) #Probability of obtaining a t-value bigger than the one that is observed (this is a vector)
  ppr=(p_larger-(1-power_est))/power_est #Conditional probability of larger t-value given that it is p<.05, pp-values
  KSD=ks.test(ppr,punif)$statistic #Kolmogorov Smirnov test on that vector against the theoretical U[0,1] distribution
  return(KSD) }
#Find the best fitting effect size (this also generates a diagnostic plot)
plotloss=function(t,df,dmin,dmax) #Syntax, same as above plus: dmin/dmax: smallest/biggest d considered,
{ loss.all=c() #Vector where results of fit for each candidate effect size are stored
  di=c() #Vector where the respective effect sizes are stored
  for (i in 0:((dmax-dmin)*100)) { #Do a loop considering every effect size between dmin and dmax in steps of .01
    d=dmin+i/100 #What effect size are we considering?
    di=c(di,d) #Add it to the vector of effect sizes
    options(warn=-1) #turn off warning because R does not like its own pt() function!
    loss.all=c(loss.all,loss(df=df,t=t,d_est=d)) #add loss for that effect size to the vector with all losses
    options(warn=0) #turn warnings back on
  }
  imin=match(min(loss.all),loss.all) #Find the attempted effect size that leads to smallest los overall
  dstart=dmin+imin/100 #Counting from dmin, what effect size is that?
  dhat=optimize(loss,c(dstart-.1,dstart+.1), df=df,t=t) #Now optimize in the neighborhood of that effect size
  #PLOT RESULTS
  plot(di,loss.all,xlab="Effect size\nCohen-d", ylab="Loss (D stat in KS test)",ylim=c(0,1), main="How well does each effect size fit? (lower is better)")
  points(dhat$minimum,dhat$objective,pch=19,col="red",cex=2) #Put a red dot in the estimated effect size
  #Add a label
  text(dhat$minimum,dhat$objective-.08,paste0("p-curve's estimate of effect size:\nd=",round(dhat$minimum,3)),col="red")
  #Convert to Pearson r. n1 n2 probably not important given size of n here.
  r = d2r(dhat$minimum)
  #output
  return(c(dhat$minimum, r))
}

# Leave-one-out Sensitivity Analysis ----
sensitive_pcurve = function(t, df, dmin, dmax) {
  outputFrame = data.frame(t, df, dhat = NA, r = NA)
  for (i in 1:length(t)) {
    outputFrame[i, c("dhat", "r")] = plotloss(t[-i], df[-i], dmin, dmax)
  }
  return(outputFrame)
}

# #Example 
# t= c(1.7, 2.8, -3.1, 2.4) # include one p>.05 and one negative t-value to highlight how we treat those
# df=c(44, 75, 125, 200)
# plotloss(t=t,df=df,dmin=-1,dmax=1)
# 



# setwd("G:/CRTT-meta/p-curve")
# dat = read.delim("ps-for-t-approx.txt")
# dat$df = dat[,1]-2
# dat$t_approx = qt(dat$p.value/2, dat$df, lower.tail=F)
# 
# plotloss(t=dat$t_approx[dat$top3=="y"], df=dat$df[dat$top3=="y"], dmin=-.8, dmax=.8) # d = .365, r = .1795
# plotloss(t=dat$t_approx[dat$top5VVG=="y"], df=dat$df[dat$top5VVG=="y"], dmin=-.8, dmax=.8) # d = .315, r = .1556
# plotloss(t=dat$t_approx[dat$top5VVG=="n"], df=dat$df[dat$top5VVG=="n"], dmin=-.8, dmax=.8) # d = .163, r = .08
# 
# # get N for d-to-r conversion 
# sum(dat$Final.N[dat$top3=="y"])
# sum(dat$Final.N[dat$top5VVG=="y"])
# sum(dat$Final.N[dat$top5VVG=="n"])

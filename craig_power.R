# Craig Anderson power analysis
# After 20 years, surely there's at least one nonsig study that should have
# been in the 2010 meta?

install.packages('pwr')
require(pwr)

dat=read.delim("Craig_Table_2010.txt", stringsAsFactors=F)
craig = dat[grep("Anderson", dat$Full.Reference),]
length(unique(craig$Full.Reference))

craig$se.r=sqrt((1-craig$Correlation)/(craig$Sample.size-2))
craig$t = craig$Correlation/craig$se.r
craig$df = craig$Sample.size-2
craig$p = 2*(1-pt(abs(craig$t), craig$df))
hist(craig$p)
View(craig)
View(craig[craig$p > .05,]) # looks like there are a fair number of nonsig effect sizes reported
# However, they are always in the 'right direction' and are probably presented alongside a sig finding
# That said, maybe not so biased after all?
table(craig$Best.) # haha his own studies are all best-practices save 3 (96%)
hist(craig$p[craig$p<.05]) # nice steep curve!

# agg beh
ab = craig[craig$Outcome == "AggBeh",]
# May be yet necessary to filter or aggregate within e.g. Longitudinal
power = pwr.r.test(ab$Sample.size, r=.21)
hist(power$power)
median(power$power)

library(plyr)
library(dplyr)
library(tidyr)
library(metafor)
library(ggplot2)


dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
source("PETPEESE_functions.R")
z2rtrans = function(z) (exp(2*z) - 1) / (exp(2*z) + 1)


dat$sig = ifelse(dat$p.twotail < .05, "significant", "not-significant")

dat = 
  dat %>%
  mutate("Diss" = ifelse(dat$Pub == "Dissertation (unpub)", "Diss", "Not Diss"))
# Brady (2006) ended up as a journal-published article.
dat$Diss[grep("Brady, S. (2006).", dat$Full.Reference, fixed=T)] = "Not Diss"
# Check count of dissertations
dat %>%
  filter(Pub == "Dissertation (unpub)") %>%
  select(Full.Reference) %>%
  distinct()
# Check count of unpub
dat %>%
  filter(Pub == "Unpublished") %>%
  select(Full.Reference, sig) %>%
  distinct() 

# Liberal test: assume independence among effect sizes
t1 = table(dat$sig, dat$Diss)
prop.test(t1)

t2 = table(dat$Best., dat$Diss)
prop.test(t2)

# Conservative test: assume perfect dependence among effect sizes
aggregate_p = function(p) ifelse(sum(p < .05) > 0, 
                                 ifelse(sum(p > .05) == 0, "all sig", "mixed sig"),
                                 "non sig")
smalldat1 = dat %>%
  group_by(Full.Reference, Study) %>%
  summarize("pattern" = aggregate_p(p.twotail),
            "Diss" = unique(Diss))
t3 = table(smalldat1$pattern, smalldat1$Diss)
t3 
prop.test(t3)

smalldat2 = dat %>% 
  distinct(Best., Full.Reference, Study)
t4 = table(smalldat2$Best., smalldat2$Diss)
t4
prop.test(t4)

dat %>% 
  filter(Setting == "Exp", !(Outcome %in% c("Empathy", "ProsBeh"))) %>% 
  ggplot(aes(x=Fisher.s.Z, y = Std.Err, col=Diss)) +
  geom_point(cex=3) +
  theme_bw() +
  scale_y_reverse()

pdf("funnel_diss.pdf", width = 8, height = 3)
par(mfrow = c(1, 3))

dat.aggAff =  dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. %in% c("y", "n"))
funnel(naiveRE(dat.aggAff), 
       pch = ifelse(dat.aggAff$Diss == "Diss", 4, 16),
       main = "Aggressive Affect",
       hlines = "grey80",
       level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = "gray90")
m1 = naiveRE(dat.aggAff)
m1
lapply(c(m1$b, m1$ci.lb, m1$ci.ub), FUN = z2rtrans)
m1.diss = naiveRE(dat.aggAff, subset = dat.aggAff$Diss == "Diss")
m1.diss
lapply(c(m1.diss$b, m1.diss$ci.lb, m1.diss$ci.ub), FUN = z2rtrans)

dat.aggBeh =  dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. %in% c("y", "n"))
funnel(naiveRE(dat.aggBeh), 
       pch = ifelse(dat.aggBeh$Diss == "Diss", 4, 16),
       main = "Aggressive Behavior",
       hlines = "grey80",
       level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = "gray90")
m2 = naiveRE(dat.aggBeh)
m2
lapply(c(m2$b, m2$ci.lb, m2$ci.ub), FUN = z2rtrans)
m2.diss = naiveRE(dat.aggBeh, subset = dat.aggBeh$Diss == "Diss")
m2.diss
lapply(c(m2.diss$b, m2.diss$ci.lb, m2.diss$ci.ub), FUN = z2rtrans)

dat.aggCog =  dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. %in% c("y", "n"))
funnel(naiveRE(dat.aggCog), 
       pch = ifelse(dat.aggCog$Diss == "Diss", 4, 16),
       main = "Aggressive Cognition",
       hlines = "grey80",
       level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
       back = "gray90")
m3 = naiveRE(dat.aggCog)
m3
lapply(c(m3$b, m3$ci.lb, m3$ci.ub), FUN = z2rtrans)
m3.diss = naiveRE(dat.aggCog, subset = dat.aggCog$Diss == "Diss")
m3.diss
lapply(c(m3.diss$b, m3.diss$ci.lb, m3.diss$ci.ub), FUN = z2rtrans)

dev.off()

# Alternatively, restrict chi-square tests to only experiments ----
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
source("PETPEESE_functions.R")
z2rtrans = function(z) (exp(2*z) - 1) / (exp(2*z) + 1)

dat$sig = ifelse(dat$p.twotail < .05, "significant", "not-significant")
dat = 
  dat %>%
  mutate("Diss" = ifelse(dat$Pub == "Dissertation (unpub)", "Diss", "Not Diss")) %>% 
  filter(Setting == "Exp")
# Brady (2006) ended up as a journal-published article.
dat$Diss[grep("Brady, S. (2006).", dat$Full.Reference, fixed=T)] = "Not Diss"
# Check count of dissertations
dat %>%
  filter(Pub == "Dissertation (unpub)") %>%
  select(Full.Reference) %>%
  distinct()
# Check count of unpub
dat %>%
  filter(Pub == "Unpublished") %>%
  select(Full.Reference, sig) %>%
  distinct() 

# Liberal test: assume independence among effect sizes
t1 = table(dat$sig, dat$Diss)
prop.test(t1)

t2 = table(dat$Best., dat$Diss)
prop.test(t2)

# Conservative test: assume perfect dependence among effect sizes
aggregate_p = function(p) ifelse(sum(p < .05) > 0, 
                                 ifelse(sum(p > .05) == 0, "all sig", "mixed sig"),
                                 "non sig")
smalldat1 = dat %>%
  group_by(Full.Reference, Study) %>%
  summarize("pattern" = aggregate_p(p.twotail),
            "Diss" = unique(Diss))
t3 = table(smalldat1$pattern, smalldat1$Diss)
t3 
prop.test(t3)

smalldat2 = dat %>% 
  distinct(Best., Full.Reference, Study)
t4 = table(smalldat2$Best., smalldat2$Diss)
t4
prop.test(t4)

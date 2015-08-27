library(dplyr)
library(tidyr)
library(metafor)
library(ggplot2)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

dat$sig = ifelse(dat$p.twotail < .05, "significant", "not-significant")

dat = 
  dat %>%
  mutate("Diss" = ifelse(dat$Pub == "Dissertation (unpub)", "Diss", "Not Diss"))
# Brady (2006) is already included as a peer-reviewed journal article
dat$Diss[grep("Brady, S. (2006).", dat$Full.Reference, fixed=T)] = "Not Diss"


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
            "Diss" = Diss)
t3 = table(smalldat1$pattern, smalldat1$Diss)
prop.test(t3)

smalldat2 = dat %>% 
  distinct(Best., Full.Reference, Study)
t4 = table(smalldat2$Best., smalldat2$Diss)
prop.test(t4)

dat %>% 
  filter(Setting == "Exp", !(Outcome %in% c("Empathy", "ProsBeh"))) %>% 
  ggplot(aes(x=Fisher.s.Z, y = Std.Err, col=Diss)) +
  geom_point(cex=3) +
  theme_bw() +
  scale_y_reverse()

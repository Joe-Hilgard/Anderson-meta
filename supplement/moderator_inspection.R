# addressing reviewer comments
library(dplyr)
library(ggplot2)
library(metafor)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# correlation between moderators ----
# AggBeh
dat %>% 
  filter(Setting == "Exp",
         Outcome == "AggBeh") %>% 
  mutate(best = ifelse(Best. == "y", 1, 0),
         age = ifelse(AGE == "Adult", 1, ifelse(AGE == "Child", 0, NA)),
         person = ifelse(PERSON == "First", 1, 0),
         east = ifelse(East.West == "East", 1, 0),
         proceeding = ifelse(Pub == "Conf. Proceedings", 1, 0),
         diss = ifelse(Pub == "Dissertation (unpub)", 1, 0)) %>% 
  select(Sample.size, Fisher.s.Z, best:diss) %>% 
  cor(use = "pairwise") %>% 
  round(3) %>% 
  write.csv(file = "./supplement/AggBeh_moderator_cortable.csv")

# AggAff
dat %>% 
  filter(Setting == "Exp",
         Outcome == "AggAff") %>% 
  mutate(best = ifelse(Best. == "y", 1, 0),
         age = ifelse(AGE == "Adult", 1, ifelse(AGE == "Child", 0, NA)),
         person = ifelse(PERSON == "First", 1, 0),
         east = ifelse(East.West == "East", 1, 0),
         proceeding = ifelse(Pub == "Conf. Proceedings", 1, 0),
         diss = ifelse(Pub == "Dissertation (unpub)", 1, 0)) %>% 
  select(Sample.size, Fisher.s.Z, best:diss) %>% 
  cor(use = "pairwise") %>% 
  round(3) %>% 
  write.csv(file = "./supplement/AggAff_moderator_cortable.csv")

# AggCog
dat %>% 
  filter(Setting == "Exp",
         Outcome == "AggCog") %>% 
  mutate(best = ifelse(Best. == "y", 1, 0),
         age = ifelse(AGE == "Adult", 1, ifelse(AGE == "Child", 0, NA)),
         person = ifelse(PERSON == "First", 1, 0),
         east = ifelse(East.West == "East", 1, 0),
         proceeding = ifelse(Pub == "Conf. Proceedings", 1, 0),
         diss = ifelse(Pub == "Dissertation (unpub)", 1, 0)) %>% 
  select(Sample.size, Fisher.s.Z, best:diss) %>% 
  cor(use = "pairwise") %>% 
  round(3) %>% 
  write.csv(file = "./supplement/AggCog_moderator_cortable.csv")

# Funnel plots ----
# AggBeh
dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = PERSON, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse()

dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = AGE, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse()

# AggAff
dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = PERSON, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse()

dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = AGE, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse() 
# Looks like a lot of "not-best" studies used children

# AggCog
dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = PERSON, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse()

dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog") %>% 
  ggplot(aes(x = Fisher.s.Z, y = Std.Err, col = AGE, shape = Best.)) +
  geom_point(size = 4) +
  scale_y_reverse() 

# Conduct models ----
# AggBeh
m1 = dat %>% 
  filter(Setting == "Exp", Outcome == "AggBeh", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, weights = 1/Std.Err,
         mods = Fisher.s.Z ~ Std.Err + AGE + PERSON + East.West, data = .)
summary(m1)

# AggAff
m2 = dat %>% 
  filter(Setting == "Exp", Outcome == "AggAff", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, weights = 1/Std.Err,
      mods = Fisher.s.Z ~ Std.Err + AGE + PERSON + East.West, data = .)
summary(m2)

# AggCog
m3 = dat %>% 
  filter(Setting == "Exp", Outcome == "AggCog", Best. == "y") %>% 
  rma(yi = Fisher.s.Z, sei = Std.Err, weights = 1/Std.Err,
      mods = Fisher.s.Z ~ Std.Err + AGE + PERSON + East.West, data = .)
summary(m3)
# P-uniform estimates

# Read in the data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

#devtools::install_github("RobbievanAert/puniform")
library(puniform)
library(dplyr)
library(magrittr)

# Container for exported values
out = data.frame("Outcome" = NULL,
                 "Setting" = NULL,
                 "Best" = NULL,
                 "est" = NULL, 
                 "ci.lb" = NULL, 
                 "ci.ub" = NULL, 
                 "pval" = NULL)

# TODO: sensitivity analyses

# Aggressive affect ----
# Best experiments
sel = dat %>% 
  filter(Outcome == "AggAff", Setting == "Exp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggAff", "Setting" = "Exp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
  
# All experiments
sel = dat %>% 
  filter(Outcome == "AggAff", Setting == "Exp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggAff", "Setting" = "Exp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# Best cross-sections
sel = dat %>% 
  filter(Outcome == "AggAff", Setting == "Nonexp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggAff", "Setting" = "Nonexp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# All cross-sections
sel = dat %>% 
  filter(Outcome == "AggAff", Setting == "Nonexp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggAff", "Setting" = "Nonexp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# Aggressive Behavior ----
# Best experiments
sel = dat %>% 
  filter(Outcome == "AggBeh", Setting == "Exp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P") 
out = bind_rows(out, 
                data.frame("Outcome" = "AggBeh", "Setting" = "Exp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# All experiments
sel = dat %>% 
  filter(Outcome == "AggBeh", Setting == "Exp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggBeh", "Setting" = "Exp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# Best cross-sections
sel = dat %>% 
  filter(Outcome == "AggBeh", Setting == "Nonexp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggBeh", "Setting" = "Nonexp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# All cross-sections
sel = dat %>% 
  filter(Outcome == "AggBeh", Setting == "Nonexp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggBeh", "Setting" = "Nonexp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# Aggressive Cognition
# Best experiments
sel = dat %>% 
  filter(Outcome == "AggCog", Setting == "Exp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggCog", "Setting" = "Exp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# All experiments
sel = dat %>% 
  filter(Outcome == "AggCog", Setting == "Exp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggCog", "Setting" = "Exp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# Best cross-sections
sel = dat %>% 
  filter(Outcome == "AggCog", Setting == "Nonexp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggCog", "Setting" = "Nonexp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# All cross-sections
sel = dat %>% 
  filter(Outcome == "AggCog", Setting == "Nonexp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "AggCog", "Setting" = "Nonexp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# Physiological arousal ----
# Best experiments
sel = dat %>% 
  filter(Outcome == "PhysArous", Setting == "Exp", Best. == "y")
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "PhysArous", "Setting" = "Exp", "Best" = "Best-only",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))
# All experiments
sel = dat %>% 
  filter(Outcome == "PhysArous", Setting == "Exp", Best. %in% c("y", "n"))
mod = puniform(ri = sel$Correlation, ni = sel$Sample.size,
         alpha = .025, side = "right", method = "P")
out = bind_rows(out, 
                data.frame("Outcome" = "PhysArous", "Setting" = "Exp", "Best" = "All",
                           mod$est, mod$ci.lb, mod$ci.ub, mod$pval.0, mod$pval.pb))

# TODO: prune rows where k < 10
# TODO: Add p-uniform's test for (publication?) bias
out = out %>% 
  mutate_each(funs(round(., 3)), mod.est:mod.pval.pb)
out$mod.pval.0[out$mod.pval.0 == 0] = "< .001"
write.table(out, "puniform_results.txt", sep = "\t")
# TODO: Implement sensitivity analyses, outlier detection
# TODO: Curious that for many outcomes the p-uniform estimate is higher than the FE estimate.
# e.g. Ballard & Wiest (1998) outlier in aggressive affect
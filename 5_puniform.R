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

# Sensitivity analyses ----
# TODO: Implement sensitivity analyses, outlier detection
# e.g. Ballard & Wiest (1998) outlier in aggressive affect

# conduct and export sensitivity analysis
# Leave-one-out Sensitivity function
sensitive_punif = function(ri, ni, ...) {
  outputFrame = data.frame(ri, ni, rhat = NA, bias.test = NA)
  for (i in 1:length(ri)) {
    temp = puniform(ri = ri[-i],
                    ni = ni[-i],
                    ...)
    outputFrame$rhat[i] = temp$est
    outputFrame$bias.test[i] = temp$pval.pb
  }
  return(outputFrame)
}

dir.create("./sensitivity_analyses/")
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) { # "Long" haven't been cleaned, prob not enough for PETPEESE
    for (k in 1:2) { # Craig didn't look at not-best separately but rolled them in
      best = list("y", c("n", "y"))
      set = dat %>% 
        filter(Outcome == i, Setting == j, Best. %in% best[[k]])
      if (nrow(set) < 10) next
      
      # Run leave-one-out analyses
      sensFrame = cbind(Study.name = as.character(set$Study.name), 
                        sensitive_punif(set$Correlation, set$Sample.size,
                                        alpha = .025, side = "right", method = "P"))
      # Combine with raw data, ID vars
      IDFrame = set %>% 
        select(Study.name, Full.Reference, 
               Correlation, Fisher.s.Z)
      sensFrame = right_join(IDFrame,
                             sensFrame,
                             by = "Study.name")
      
      # Export analysis
      write.table(sensFrame, 
                  file = paste0("./sensitivity_analyses/punif_", paste(i, j, k, sep="_"), ".txt"),
                  sep = "\t",
                  row.names = F)
      
    }
  }
}

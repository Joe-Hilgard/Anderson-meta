# Anderson_2010 master script

# Run these two lines once per machine to install required packages.
# install.packages(c("plyr", "dplyr", "readxl", "ggplot2", "tidyr", "metafor", "devtools", "magrittr", "pwr"))
# devtools::install_github("RobbievanAert/puniform")

source("1_cleaning.R")
source("2_PET-PEESE.R")
source("3_pcurve.R")
source("4_dissertations.R")
source("5_puniform.R")
source("6_TES.R")

# aggregate results & make tables ----
library(dplyr)
results1 = read.delim("PETPEESE_output.txt")
results2 = read.delim("pcurve_results.txt")
results3 = read.delim("puniform_results.txt")
results4 = read.delim("TES_results.txt")

results = full_join(results2, results1) %>% 
  full_join(results3) %>% 
  full_join(results4) %>% 
  tbl_df()

results = results %>% 
  mutate("pcurve.rhat" = round(rhat_pcurve, 3)) %>% 
  select(-(dhat_pcurve:rhat_pcurve))

dir.create("./results/")

write.table(results, file = "./results/full_results.txt", sep="\t", row.names=F)

# Aggregate sensitivity analyses ----
# Nevermind with this for now, I'm having trouble with it.
# sensOutMaster = NULL # holster object
# sensOut = NULL
# 
# for (l in c("meta", "pcurve", "punif", "TES")) {
#   for (i in c("AggAff", "AggBeh", "AggCog", "PhysArous")) {
#     for (j in c("Exp", "Nonexp")) {
#       for (k in 1:2) {
#         
#         Best = c("Best-only", "All")[k]
#         
#         # Label which analysis it is
#         sensDat = data.frame("Outcome" = i,
#                              "Setting" = j,
#                              "Best" = Best)
#         # Read in the sensitivity analyses
#         fileName = paste(l, i, j, k, sep = "_") %>% 
#           paste0(".txt")
#         # skip if the file doesn't exist
#         fileExist = list.files(path = "./sensitivity_analyses/", pattern = fileName)
#         if (length(fileExist) == 0) next
#         # Combine the labels & the data
#         sensDat = cbind(sensDat, 
#                         read.delim(paste0("./sensitivity_analyses/", fileName)))
#         
#         # Add the new rows to the exported object
#         sensOut = bind_rows(sensOut, sensDat)
#       }
#     }
#   }
#   
#   # if sensOutMaster doesn't exist yet, make it equal to the export
#   if (is.null(sensOutMaster)) {
#     sensOutMaster = sensOut
#   }
#   # Otherwise staple it onto what exists already
#   if (!is.null(sensOutMaster)) {
#     sensOutMaster = full_join(sensOutMaster, sensOut)
#   } 
# }
# 
# write.table(sensOut, "./sensitivity_analyses/1_master_sensitivity.txt", row.names = F, sep = "\t")

# results %>% 
#   select(Outcome:Best, )

Round = function(x, digits = 2) format(round(x, digits), nsmall = digits)
confint_pretty = function(point, ll, ul, digits = 2) {
  paste0(Round(point, digits), " [", Round(ll, digits), ", ", Round(ul, digits), "]") %>% 
    return
}

# Table 1
results %>% 
  select(Outcome, Setting, Best,
         PET.b1, PET.b1.se, PET.b1.p, mod.pval.pb, TES.pval) %>% 
  rename(Egger.coef = PET.b1,
         SE.Egger.coef = PET.b1.se,
         Egger.p = PET.b1.p,
         p.uniform = mod.pval.pb) %>% 
  write.table("./results/bias_tests.txt", sep = "\t", row.names = F)

# Table 2
results %>% 
  mutate("Naive.FE" = confint_pretty(naive.FE.r, naive.FE.r.lb, naive.FE.r.ub),
         "Naive.RE" = confint_pretty(naive.RE.r, naive.RE.r.lb, naive.RE.r.ub),
         "Naive.I2" = confint_pretty(RE.I2, RE.I2.lb, RE.I2.ub, digits = 0),
         "PET" = confint_pretty(PET.r, PET.r.LL, PET.r.UL),
         "PET.I2" = confint_pretty(PET.I2, PET.I2.lb, PET.I2.ub, digits = 0),
         "PEESE"    = confint_pretty(PEESE.r, PEESE.r.LL, PEESE.r.UL),
         "PEESE.I2" = confint_pretty(PEESE.I2, PEESE.I2.lb, PEESE.I2.ub, digits = 0),
         "p.uniform" = confint_pretty(mod.est, mod.ci.lb, mod.ci.ub),
         "pcurve.rhat" = round2(pcurve.rhat)
         ) %>% 
  select(Outcome, Setting, Best, k, n,
         Naive.FE, Naive.RE, Naive.I2,
         PET, PET.I2,
         PEESE, PEESE.I2,
         p.uniform,
         pcurve.rhat) %>% 
  write.table("./results/estimates.txt", sep = "\t", row.names = F)

# make demonstration funnels
# source("funnel_demos.R")


# export plots in publication-ready format ----
source("7_plotting.R")
# Et voila! ----
cat("And that's a wrap!")

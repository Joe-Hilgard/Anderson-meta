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
source("7_plotting.R")
source("8_trimfill.R")

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
         "pcurve.rhat" = Round(pcurve.rhat)
         ) %>% 
  select(Outcome, Setting, Best, k, n,
         Naive.FE, Naive.RE, Naive.I2,
         PET, PET.I2,
         PEESE, PEESE.I2,
         p.uniform,
         pcurve.rhat) %>% 
  write.table("./results/estimates.txt", sep = "\t", row.names = F)

# Et voila! ----
cat("And that's a wrap!")

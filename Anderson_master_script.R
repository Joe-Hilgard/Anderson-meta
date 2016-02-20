# Anderson_2010 master script

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

write.table(results, file = "full_results.txt", sep="\t", row.names=F)

results %>% 
  select(Outcome:n, RE.I2, PET.I2, PEESE.I2)

results %>% 
  select(Outcome, Setting, Best, 
         naive.RE.r, PET.r, PEESE.r, mod.est, rhat_pcurve) %>% 
  View

# make demonstration funnels
# source("funnel_demos.R")

# export plots in publication-ready format ----
source("7_plotting.R")
# Et voila! ----
cat("And that's a wrap!")

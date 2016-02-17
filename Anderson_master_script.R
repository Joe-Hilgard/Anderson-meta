# Anderson_2010 master script

source("Anderson_cleaning.R")
source("Anderson_2010_PET-PEESE.R")
source("Anderson_pcurve.R")
source("Anderson_dissertations.R")
source("p-uniform.R")

# aggregate results & make tables ----
library(dplyr)
results1 = read.delim("PETPEESE_output.txt")
results2 = read.delim("pcurve_results.txt")
results3 = read.delim("puniform_results.txt")

results = full_join(results2, results1) %>% 
  full_join(results3) %>% 
  tbl_df()

write.table(results, file = "full_results.txt", sep="\t", row.names=F)

results %>% 
  select(Outcome:n, RE.I2, PET.I2, PEESE.I2)

results %>% 
  select(Outcome:n, naive.RE.r, mod.est, rhat_pcurve)

# make demonstration funnels
# source("funnel_demos.R")

# export plots in publication-ready format ----
source("Anderson_plotting.R")
# Et voila! ----
cat("And that's a wrap!")

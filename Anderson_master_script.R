# Anderson_2010 master script

source("Anderson_cleaning.R")
source("Anderson_2010_PET-PEESE.R")
source("Anderson_pcurve.R")
source("Anderson_dissertations.R")
source("p-uniform.R")

# aggregate results ----
library(dplyr)
results1 = read.delim("PETPEESE_output.txt")
results2 = read.delim("pcurve_results.txt")
results3 = read.delim("puniform_results.txt")
full_join(results2, results1) %>% 
  full_join(results3) %>% 
  write.table(file = "full_results.txt", sep="\t", row.names=F)

# make demonstration funnels
# source("funnel_demos.R")

# export plots in publication-ready format ----
source("Anderson_plotting.R")
# Et voila! ----
cat("And that's a wrap!")

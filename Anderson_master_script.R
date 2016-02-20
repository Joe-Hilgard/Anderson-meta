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

# Aggregate sensitivity analyses ----
sensOut = NULL # holster object

# TODO: Something's wrong here with control flow of loop.
for (i in c("AggAff", "AggBeh", "AggCog", "PhysArous")) {
  for (j in c("Exp", "Nonexp")) {
    for (k in 1:2) {
      for (l in c("meta", "pcurve", "punif", "TES")) {
        Best = c("Best-only", "All")[k]
        
        # Label which analysis it is
        sensDat = data.frame("Outcome" = i,
                             "Setting" = j,
                             "Best" = Best)
        # Read in the sensitivity analyses
        fileName = paste(l, i, j, k, sep = "_") %>% 
          paste0(".txt")
        # skip if the file doesn't exist
        fileExist = list.files(path = "./sensitivity_analyses/", pattern = fileName)
        if (length(fileExist) == 0) next
        # Combine the labels & the data
        sensDat = cbind(sensDat, 
                        read.delim(paste0("./sensitivity_analyses/", fileName)))
        
        # Add the new rows to the exported object
        sensOut = bind_rows(sensOut, sensDat)
      }
    }
  }
}

write.table(sensOut, "./sensitivity_analyses/1_master_sensitivity.txt", row.names = F, sep = "\t")

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

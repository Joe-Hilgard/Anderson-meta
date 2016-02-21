# Test of excess significance
library(pwr)
library(dplyr)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat$sig = ifelse(dat$p.twotail < .05, "significant", "not-significant")

# From Ioannidis & Trikalinos, p. 246
# Expected # of sig results is sum of studies' powers
# (We use meta-analytic estimate r = .21 as hypothesized ES)
TES = function(dataset) {
  modelNaiveFE = rma(yi = Fisher.s.Z, sei = Std.Err, method = "FE", data = dataset)
  r = tanh(modelNaiveFE$b[1])
  # generate power estimates
  for (i in 1:nrow(dataset)) {
    dataset$power[i] = pwr.r.test(n=dataset$Sample.size[i],
                                  r=r,
                                  alternative = "two.sided")$power
  }
  plot(dataset$Sample.size, dataset$power)  
  # Conduct TES
  n = nrow(dataset) # n is number of studies
  E = sum(dataset$power) # E is expected # of sig results, sum of power
  O = sum(dataset$sig == "significant") # O is observed # of sig results
  # Statistic is 
  A = (O-E)^2/E + (O-E)^2/(n-E)
  # Test A for statistical significance
  pchisq(A, 1, lower.tail = F) %>% 
    round(3) %>% 
    return()
}

# Leave-one-out sensitivity analysis for TES
sensitive_TES = function(dataset) {
  outputFrame = data.frame("Fisher.s.Z" = dataset$Fisher.s.Z, 
                           "Std.Err" = dataset$Std.Err, 
                           TES.pval = NA)
  for (i in 1:length(dataset$Fisher.s.Z)) {
    outputFrame[i, c("TES.pval")] = TES(dataset[-i,])
  }
  return(outputFrame)
}

# Make output data frame
# out = expand.grid("Best" = c("Best", "Full"),
#                   "Outcome" = c("AggAff", "AggBeh", "AggCog", "PhysArous"),
#                   "Setting" = c("Exp", "NonExp"))
# # Sort and clean it
# out = out %>% 
#   filter(!(Outcome == "PhysArous" & Setting == "NonExp")) %>% 
#   select(Setting, Outcome, Best)
# out$TES.p = NULL

# Prepare holster
out = data.frame(NULL)

# Make directory for sensitivity analyses
dir.create("sensitivity_analyses")

# Triple loop to run all analyses, sensitivity analyses.
for (i in unique(dat$Outcome)) {
  for (j in c("Exp", "Nonexp")) {
    for (k in 1:2) {
      best = list("y", c("n", "y"))
      set = dat %>% 
        filter(Outcome == i, Setting == j, Best. %in% best[[k]])
      if (nrow(set) < 10) next
      
      # Conduct TES
      out_1run = data.frame("Outcome" = i,
                            "Setting" = j,
                            "Best" = c("Best-only", "All")[k],
                            "TES.pval" = TES(set))
      # Append to previous
      out = rbind(out, out_1run)
      
      # Run leave-one-out sensitivity analyses
      sensFrame = cbind(Study.name = as.character(set$Study.name), 
                        sensitive_TES(set))
      
      # Combine sensitivity analyses with labels
      IDFrame = set %>% 
        select(Study.name, Full.Reference, 
               Correlation, Fisher.s.Z)
      sensFrame = right_join(IDFrame,
                             sensFrame,
                             by = "Study.name")
      
      # Export sensitivity analyses
      write.table(sensFrame, 
                  file = paste0("./sensitivity_analyses/TES_", paste(i, j, k, sep="_"), ".txt"),
                  sep = "\t",
                  row.names = F)
      
    }
  }
}

# Export primary analyses
write.table(out, file = "TES_results.txt", sep = "\t", row.names = F)

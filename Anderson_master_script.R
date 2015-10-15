# Anderson_2010 master script

source("Anderson_cleaning.R")
source("Anderson_2010_PET-PEESE.R")
source("Anderson_pcurve.R")
source("Anderson_dissertations.R")

library(dplyr)
results1 = read.delim("PETPEESE_output.txt")
results2 = read.delim("pcurve_results.txt")
full_join(results2, results1) %>% 
  write.table(file = "full_results.txt", sep="\t", row.names=F)

# appraise similarities between SE and VAR as moderators
  # vs. 1/sqrt(N) and 1/N
# d1 = read.table("PETPEESE_output.txt", header=T)
# d2 = read.table("peters.txt", header=T)
# hist(d1$PET.b0 - d2$peters_linear.b0) # Very little difference
# hist(d1$PET.b1 - d2$peters_linear.b1) # Very little difference
# hist(d1$PEESE.b0 - d2$peters_quadratic.b0) # Very little difference

# overlay
# plot(d1$PET.r, col="red", pch = 16, ylim = c(-.3, .5))
# points(d1$PET.r.LL, col="red")
# points(d1$PET.r.UL, col="red")
# points(d2$peters_linear.r, col="darkgreen", pch=16)
# points(d2$peters_linear.r.LL, col="darkgreen", cex=1.5)
# points(d2$peters_linear.r.UL, col="darkgreen", cex=1.5)
# # Similar results except for row 12.
# 
# # For any reasonable N, my PET and PEESE should be
#   # giving essentially the same estimate b/c
#   # SE(z) was not a function of z.
# x = 10:200
# plot(1/sqrt(x), 1/sqrt(x-3))

# Read in data
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# Load dplyr package
library(dplyr)

# Select random subset of 15 experiments
set.seed(1)
temp = dat %>% 
  filter(Setting == "Exp", 
         Outcome %in% c("AggBeh", "AggAff", "AggCog"), 
         Pub == "Journal",
         Country != "Japan") %>% 
  select(Study, Study.name, Outcome, Full.Reference, Correlation, Sample.size, Std.Err, Fisher.s.Z) %>% 
  sample_n(15, replace = F)

# Export to Excel
write.table(temp, "./supplement/se_z_approximation.csv", sep = ",", row.names=F)

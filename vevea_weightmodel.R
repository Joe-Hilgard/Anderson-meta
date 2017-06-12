# moderate and severe selection models used by Kepes et al.

library(readxl)
library(tidyverse)
weights <- read_excel("vevea_weights.xlsx") %>% 
  gather(key = model, value = weight, mod.onetail:severe.twotail)

ggplot(weights, aes(x = min.p, y = weight, col = model)) +
  geom_point() + 
  geom_line()

# Doesn't work yet
# make dataframe iterating over 0 < p < 1 in steps of .001
# weightframe <- data.frame(expand.grid(p = seq(0, 1, .001),
#                                       model = c("mod.onetail", "sev.onetail", "mod.twotail", "sev.twotail")))
# weightframe$weight <- NULL
# for (i in 1:length(weightframe)) {
#   weightframe$weight[i] <- weights$weight[weights$model[i] == weightframe$model &
#                                             weights$min.p[i] >= weightframe$p &
#                                             weights$max.p[i] < weightframe$p]
# }

# inspect
# ggplot(weightframe, aes(x = p, y = weight, col = model)) +
#   geom_point() +
#   geom_line()



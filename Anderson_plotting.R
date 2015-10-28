# Anderson plots
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
source("PETPEESE_functions.R")
library(dplyr)
library(magrittr)
# Funnel plots centered at naive estimate, no contours ----
# Affect ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F,
                 xlim = c(-.5, 1.5), ylim = c(.32, 0))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F, 
                 xlim = c(-.1, .6), ylim = c(.1, 0))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F, 
                 xlim = c(-.5, 1.5), ylim = c(.32, 0))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best cross-sectional", printText = F,
                 xlim = c(-.1, .6), ylim = c(.1, 0))
savePlot(filename="funnels_AggAff.pdf", type = "pdf")
dev.off()

# Behavior ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F,
                 xlim = c(-.4, .8), ylim = c(.3, 0))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F,
                 xlim = c(-.2, .6), ylim = c(.185, 0))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F,
                 xlim = c(-.4, .8), ylim = c(.3, 0))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best cross-sectional", printText = F,
                 xlim = c(-.2, .6), ylim = c(.185, 0))
savePlot(filename="funnels_AggBeh.pdf", type = "pdf")
dev.off()

# Cognition ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F,
                 xlim = c(-.5, 1), ylim = c(.315, 0))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All cross-sectional", printText = F,
                 xlim = c(-.1, .6), ylim = c(.125, 0))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F,
                 xlim = c(-.5, 1), ylim = c(.315, 0))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best cross-sectional", printText = F,
                 xlim = c(-.1, .6), ylim = c(.125, 0))
savePlot(filename="funnels_AggCog.pdf", type = "pdf")
dev.off()

# Arousal ----
windows(width = 4, height = 8)
par(mfrow = c(2, 1))
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  funnelPETPEESE(plotName = "All experiments", printText = F,
                 xlim = c(-.5, 1), ylim = c(.41, 0))
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  funnelPETPEESE(plotName = "Best experiments", printText = F,
                 xlim = c(-.5, 1), ylim = c(.41, 0))
savePlot(filename="funnels_PhysArous.pdf", type = "pdf")
dev.off()

# Funnel plots centered at 0, significance contours----
# Affect ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All experiments",
         xlim = c(-.5, 1.5), ylim = c(.32, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All cross-sectional",
         xlim = c(-.2, .6), ylim = c(.1, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best experiments",
         xlim = c(-.5, 1.5), ylim = c(.32, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggAff", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best cross-sectional",
         xlim = c(-.2, .6), ylim = c(.1, 0)) %$%
  abline(v = b, lty = 2)
savePlot(filename="funnels-0_AggAff.pdf", type = "pdf")
dev.off()

# Behavior ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All experiments",
         xlim = c(-.6, .6), ylim = c(.30, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All cross-sectional",
         xlim = c(-.4, .6), ylim = c(.19, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best experiments",
         xlim = c(-.6, .6), ylim = c(.30, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best cross-sectional",
         xlim = c(-.4, .6), ylim = c(.19, 0)) %$%
  abline(v = b, lty = 2)
savePlot(filename="funnels-0_AggBeh.pdf", type = "pdf")
dev.off()

# Cognition ----
windows(width = 8, height = 8)
par(mfrow = c(2, 2))
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All experiments",
         xlim = c(-.2, .8), ylim = c(.315, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All cross-sectional",
         xlim = c(-.4, .6), ylim = c(.125, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best experiments",
         xlim = c(-.2, .8), ylim = c(.315, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "AggCog", 
         Setting == "Nonexp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best cross-sectional",
         xlim = c(-.4, .6), ylim = c(.125, 0)) %$%
  abline(v = b, lty = 2)
savePlot(filename="funnels-0_AggCog.pdf", type = "pdf")
dev.off()

# Arousal ----
windows(width = 4, height = 8)
par(mfrow = c(2, 1))
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y", "n")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "All experiments",
         xlim = c(-1, 1), ylim = c(.41, 0)) %$%
  abline(v = b, lty = 2)
dat %>%
  filter(Outcome == "PhysArous", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  naive() %T>% 
  funnel(level=c(90, 95, 99), shade = c("white", "grey75", "grey60"), refline=0,
         back = "gray90",
         main = "Best experiments",
         xlim = c(-1, 1), ylim = c(.41, 0)) %$%
  abline(v = b, lty = 2)
savePlot(filename="funnels-0_PhysArous.pdf", type = "pdf")
dev.off()

# Double funnel plots with 0-reference and naive-centered funnels ----

# Behavior
MA = dat %>%
  filter(Outcome == "AggBeh", 
         Setting == "Exp",
         Best. %in% c("y")) %>%
  naive() 
funnel(MA, level=c(90, 95, 99), 
       shade = c("grey98", "grey55", "grey75"), refline=0,
       back = NULL, hlines = NULL,
       main = "'Best' experiments",
       xlim = c(-.6, .8), ylim = c(.30, 0))
par(new=T)
funnel(MA, xlim = c(-.6, .8), ylim = c(.30, 0), shade=NULL, back=NULL, hlines=NULL,
       xlab=NA, ylab=NA, main=NA)
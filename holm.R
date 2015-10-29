# Holm's step-down procedure

p = c(.001, .001, .048, .005, .248, .021, .001, .055, .107, .013, .001, .466, .039)
p = p[order(p)]

alpha = .10
m = length(p)
holm = alpha/(m + 1 - 1:m)

data.frame("p" = p, "k" = 1:m, "p(k)" = holm, "test" = p>holm)

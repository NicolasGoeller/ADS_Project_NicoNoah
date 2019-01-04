library(lme4)
library(sjstats)


m01 <- lmer(sat ~ 1 + (1|cntry), EVS)
summary(m01)
icc(m01)

m02 <- lmer(sat ~ 1 + (1|state), EVS)
summary(m02)
icc(m02)

m03 <- lmer(sat ~ 1 + (1|reg), EVS)
summary(m03)
icc(m03)

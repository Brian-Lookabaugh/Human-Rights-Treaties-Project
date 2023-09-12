############################################################################
###############--------------Sensitivity Analysis-------------##############
############################################################################

library(sensemakr) # Sensitivity Analysis

### Estimate the Two-Way Fixed Effects Models for Each Outcome
phy.twfe <- lm(p.vio ~ ICCPR + polity + cso + lmilper + lt.bal +
                 as.factor(ccode) + as.factor(year),
                data = final)

tor.twfe <- lm(torture ~ CAT + polity + cso + lmilper + lt.bal +
                 as.factor(ccode) + as.factor(year),
               data = final)

pol.twfe <- lm(w.pol.emp ~ CEDAW + polity + cso + lmilper + lt.bal +
                 as.factor(ccode) + as.factor(year),
               data = final)

civ.twfe <- lm(w.civ.lib ~ CEDAW + polity + cso + lmilper + lt.bal +
                 as.factor(ccode) + as.factor(year),
               data = final)

# Run the Sensitivity Analysis for Each Model
phy.sens <- sensemakr(model = phy.twfe,
                      treatment = "ICCPR",
                      benchmark_covariates = "polity",
                      kd = 1:3,
                      ky = 1:3,
                      q = 1,
                      reduce = TRUE)

tor.sens <- sensemakr(model = tor.twfe,
                      treatment = "CAT",
                      benchmark_covariates = "polity",
                      kd = 1:3,
                      ky = 1:3,
                      q = 1,
                      reduce = TRUE)

pol.sens <- sensemakr(model = pol.twfe,
                      treatment = "CEDAW",
                      benchmark_covariates = "polity",
                      kd = 1:3,
                      ky = 1:3,
                      q = 1,
                      reduce = TRUE)

civ.sens <- sensemakr(model = civ.twfe,
                      treatment = "CEDAW",
                      benchmark_covariates = "polity",
                      kd = 1:3,
                      ky = 1:3,
                      q = 1,
                      reduce = TRUE)
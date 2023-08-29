############################################################################
###################--------------Estimation-------------####################
############################################################################

library(MatchIt) # Coarsened Exact Matching
library(marginaleffects) # Estimates

# Convert Trade Balance to Log-Transformation
final <- final %>%
  mutate(t.bal = t.bal + 882207) %>%
  mutate(lt.bal = log(t.bal + 1))

### Regression Adjustment (Hill's Controls)
phy.ra.hill <- lm(p.vio ~ ICCPR * (polity + lgdppc + cso + inter.war + intra.war + 
                    lpop + h.court), data = final)
avg_comparisons(phy.ra.hill,
                variables = "ICCPR",
                vcov = ~ccode,
                newdata = subset(final, ICCPR == 1))

tor.ra.hill <- lm(torture ~ CAT * (polity + lgdppc + cso + inter.war + intra.war + 
                    lpop +  h.court), data = final)
avg_comparisons(tor.ra.hill,
                variables = "CAT",
                vcov = ~ccode,
                newdata = subset(final, CAT == 1))

pol.ra.hill <- lm(w.pol.emp ~ CEDAW * (polity + lgdppc + cso + inter.war + intra.war + 
                    lpop +  h.court), data = final)
avg_comparisons(pol.ra.hill,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(final, CEDAW == 1))

civ.ra.hill <- lm(w.civ.lib ~ CEDAW * (polity + lgdppc + cso + inter.war + intra.war + 
                    lpop + h.court), data = final)
avg_comparisons(civ.ra.hill,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(final, CEDAW == 1))

### Regression Adjustment (DAG Controls)
phy.ra.dag <- lm(p.vio ~ ICCPR * (polity + cso + lmilper + lt.bal), data = final)
avg_comparisons(phy.ra.dag,
                variables = "ICCPR",
                vcov = ~ccode,
                newdata = subset(final, ICCPR == 1))

tor.ra.dag <- lm(torture ~ CAT * (polity + cso + lmilper + lt.bal), data = final)
avg_comparisons(tor.ra.dag,
                variables = "CAT",
                vcov = ~ccode,
                newdata = subset(final, CAT == 1))

pol.ra.dag <- lm(w.pol.emp ~ CEDAW * (polity + cso + lmilper + lt.bal), data = final)
avg_comparisons(pol.ra.dag,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(final, CEDAW == 1))

civ.ra.dag <- lm(w.civ.lib ~ CEDAW * (polity + cso + lmilper + lt.bal), data = final)
avg_comparisons(civ.ra.dag,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(final, CEDAW == 1))

# Remove All NA Values
final.na <- na.omit(final)

### Coarsened Exact Matching (Hill's Controls)
phy.cem.hill <- matchit(ICCPR ~ polity + lgdppc + cso + inter.war + intra.war + 
                          lpop + h.court,
                        data = final.na,
                        cutpoints = list(polity = 5, cso = 4, h.court = 3,
                                         lgdppc = 5, lpop = 5),
                        method = "cem")
m.data1 <- match.data(phy.cem.hill)
fit1 <- lm(p.vio ~ ICCPR * (polity + lgdppc + cso + inter.war + intra.war + 
                              lpop + h.court), data = m.data1, weights = weights)
avg_comparisons(fit1,
                variables = "ICCPR",
                vcov = ~ccode,
                newdata = subset(m.data1, ICCPR == 1))

tor.cem.hill <- matchit(CAT ~ polity + lgdppc + cso + inter.war + intra.war + 
                          lpop + h.court,
                        data = final.na,
                        cutpoints = list(polity = 5, cso = 4, h.court = 3,
                                         lgdppc = 5, lpop = 5),
                        method = "cem")
m.data2 <- match.data(tor.cem.hill)
fit2 <- lm(torture ~ CAT * (polity + lgdppc + cso + inter.war + intra.war + 
                              lpop + h.court), data = m.data2, weights = weights)
avg_comparisons(fit2,
                variables = "CAT",
                vcov = ~ccode,
                newdata = subset(m.data2, CAT == 1))

pol.cem.hill <- matchit(CEDAW ~ polity + lgdppc + cso + inter.war + intra.war + 
                          lpop + h.court,
                        data = final.na,
                        cutpoints = list(polity = 5, cso = 4, h.court = 3,
                                         lgdppc = 5, lpop = 5),
                        method = "cem")
m.data3 <- match.data(pol.cem.hill)
fit3 <- lm(w.pol.emp ~ CEDAW * (polity + lgdppc + cso + inter.war + intra.war + 
                              lpop + h.court), data = m.data3, weights = weights)
avg_comparisons(fit3,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(m.data3, CEDAW == 1))

civ.cem.hill <- matchit(CEDAW ~ polity + lgdppc + cso + inter.war + intra.war + 
                          lpop + h.court,
                        data = final.na,
                        cutpoints = list(polity = 5, cso = 4, h.court = 3,
                                         lgdppc = 5, lpop = 5),
                        method = "cem")
m.data4 <- match.data(civ.cem.hill)
fit4 <- lm(w.civ.lib ~ CEDAW * (polity + lgdppc + cso + inter.war + intra.war + 
                                  lpop + h.court), data = m.data4, weights = weights)
avg_comparisons(fit4,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(m.data4, CEDAW == 1))

### Coarsened Exact Matching (DAG Controls)
phy.cem.dag <- matchit(ICCPR ~ polity + cso + lmilper + lt.bal,
                        data = final.na,
                        cutpoints = list(polity = 5, cso = 4, lmilper = 5,
                                         lt.bal = 5),
                        method = "cem")
m.data5 <- match.data(phy.cem.dag)
fit5 <- lm(p.vio ~ ICCPR * (polity +  cso + lmilper + lt.bal), 
           data = m.data5, weights = weights)
avg_comparisons(fit5,
                variables = "ICCPR",
                vcov = ~ccode,
                newdata = subset(m.data5, ICCPR == 1))

tor.cem.dag <- matchit(CAT ~ polity + cso + lmilper + lt.bal,
                       data = final.na,
                       cutpoints = list(polity = 5, cso = 4, lmilper = 5,
                                        lt.bal = 5),
                       method = "cem")
m.data6 <- match.data(tor.cem.dag)
fit6 <- lm(torture ~ CAT * (polity +  cso + lmilper + lt.bal), 
           data = m.data6, weights = weights)
avg_comparisons(fit6,
                variables = "CAT",
                vcov = ~ccode,
                newdata = subset(m.data6, CAT == 1))

pol.cem.dag <- matchit(CEDAW ~ polity + cso + lmilper + lt.bal,
                       data = final.na,
                       cutpoints = list(polity = 5, cso = 4, lmilper = 5,
                                        lt.bal = 5),
                       method = "cem")
m.data7 <- match.data(pol.cem.dag)
fit7 <- lm(w.pol.emp ~ CEDAW * (polity +  cso + lmilper + lt.bal), 
           data = m.data7, weights = weights)
avg_comparisons(fit7,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(m.data7, CEDAW == 1))

civ.cem.dag <- matchit(CEDAW ~ polity + cso + lmilper + lt.bal,
                       data = final.na,
                       cutpoints = list(polity = 5, cso = 4, lmilper = 5,
                                        lt.bal = 5),
                       method = "cem")
m.data8 <- match.data(civ.cem.dag)
fit8 <- lm(w.civ.lib ~ CEDAW * (polity +  cso + lmilper + lt.bal), 
           data = m.data8, weights = weights)
avg_comparisons(fit8,
                variables = "CEDAW",
                vcov = ~ccode,
                newdata = subset(m.data8, CEDAW == 1))

### Panel Matching (Hill's Controls)
phy.psm.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 3
)
res1 <- PanelEstimate(sets = phy.psm.hill, data = final, number.iterations = 500)
summary(res1)

tor.psm.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 3
)
res2 <- PanelEstimate(sets = tor.psm.hill, data = final, number.iterations = 500)
summary(res2)

pol.psm.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 3
)
res3 <- PanelEstimate(sets = pol.psm.hill, data = final, number.iterations = 500)
summary(res3)

civ.psm.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 3
)
res4 <- PanelEstimate(sets = civ.psm.hill, data = final, number.iterations = 500)
summary(res4)

### Panel Matching (DAG Controls)
phy.psm.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 3
)
res5 <- PanelEstimate(sets = phy.psm.dag, data = final, number.iterations = 500)
summary(res5)

tor.psm.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 3
)
res6 <- PanelEstimate(sets = tor.psm.dag, data = final, number.iterations = 500)
summary(res6)

pol.psm.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 3
)
res7 <- PanelEstimate(sets = pol.psm.dag, data = final, number.iterations = 500)
summary(res7)

civ.psm.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 3
)
res8 <- PanelEstimate(sets = civ.psm.dag, data = final, number.iterations = 500)
summary(res8)

### Panel Weighting (Hill's Controls)
phy.psw.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 3
)
resw1 <- PanelEstimate(sets = phy.psw.hill, data = final, number.iterations = 500)
summary(resw1)

tor.psw.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 3
)
resw2 <- PanelEstimate(sets = tor.psw.hill, data = final, number.iterations = 500)
summary(resw2)

pol.psw.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 3
)
resw3 <- PanelEstimate(sets = pol.psw.hill, data = final, number.iterations = 500)
summary(resw3)

civ.psw.hill <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(lgdppc, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(inter.war, 1:3)) +
    I(lag(intra.war, 1:3)) +
    I(lag(lpop, 1:3)) +
    I(lag(h.court, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 3
)
resw4 <- PanelEstimate(sets = civ.psw.hill, data = final, number.iterations = 500)
summary(resw4)

### Panel Weighting (DAG Controls)
phy.psw.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 3
)
resw5 <- PanelEstimate(sets = phy.psw.dag, data = final, number.iterations = 500)
summary(resw5)

tor.psw.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 3
)
resw6 <- PanelEstimate(sets = tor.psw.dag, data = final, number.iterations = 500)
summary(resw6)

pol.psw.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 3
)
resw7 <- PanelEstimate(sets = pol.psw.dag, data = final, number.iterations = 500)
summary(resw7)

civ.psw.dag <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:3)) +
    I(lag(milper, 1:3)) +
    I(lag(cso, 1:3)) +
    I(lag(t.bal, 1:3)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 3
)
resw8 <- PanelEstimate(sets = civ.psw.dag, data = final, number.iterations = 500)
summary(resw8)

############################################################################
###################--------------Estimation-------------####################
############################################################################

library(PanelMatch)

### For Each Dependent Variable Matrix of Combinations is:
###                                     | 1-Year Lag | 2-Year Lag | 3-Year Lag | 
### Mahalanobis                         |            |            |            |
### Propensity Score Matching           |            |            |            |
### Propensity Score Weighting          |            |            |            |

### Data Cleaning for Panel Match
final <- final %>%
  # Remove Pre-1960 Observations
  filter(year > 1959) %>%
  # Remove Observations without a COW Code
  filter(!is.na(ccode)) %>%
  # Remove Non-Numeric Data
  select(-stateabb, -country_name) %>%
  # Convert Time and Unit ID to Integer
  mutate(ccode = as.integer(ccode)) %>%
  mutate(year = as.integer(year))
## Convert Dataset to Data.Frame
final <- as.data.frame(final)
  

############################################################################
############--------------Physical Violence as DV-------------##############
############################################################################
phy.mah.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

phy.mah.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

phy.mah.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "mahalanobis",
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
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

phy.psm.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 1
)

phy.psm.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 2
)

phy.psm.3 <- PanelMatch(
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

phy.psw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 1
)

phy.psw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "ICCPR",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "p.vio",
  restrict.control.period = 2
)

phy.psw.3 <- PanelMatch(
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

############################################################################
#################--------------Torture as DV-------------###################
############################################################################
tor.mah.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

tor.mah.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

tor.mah.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "mahalanobis",
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
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

tor.psm.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 1
)

tor.psm.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 2
)

tor.psm.3 <- PanelMatch(
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

tor.psw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 1
)

tor.psw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CAT",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "torture",
  restrict.control.period = 2
)

tor.psw.3 <- PanelMatch(
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

############################################################################
##########--------------Female Political Empowerment-------------###########
############################################################################
pol.mah.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

pol.mah.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

pol.mah.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
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
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

pol.psm.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 1
)

pol.psm.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 2
)

pol.psm.3 <- PanelMatch(
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

pol.psw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 1
)

pol.psw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.pol.emp",
  restrict.control.period = 2
)

pol.psw.3 <- PanelMatch(
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

############################################################################
#############--------------Female Civil Liberties-------------##############
############################################################################
civ.mah.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 1
)

civ.mah.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 2
)

civ.mah.3 <- PanelMatch(
  lag = 3,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "mahalanobis",
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
  use.diagonal.variance.matrix = TRUE,
  restrict.control.period = 3
)

civ.psm.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 1
)

civ.psm.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.match",
  size.match = 1,
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 2
)

civ.psm.3 <- PanelMatch(
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

civ.psw.1 <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1)) +
    I(lag(milper, 1)) +
    I(lag(cso, 1)) +
    I(lag(t.bal, 1)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 1
)

civ.psw.2 <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "ccode",
  treatment = "CEDAW",
  refinement.method = "ps.weight",
  data = final,
  covs.formula = ~
    I(lag(polity, 1:2)) +
    I(lag(milper, 1:2)) +
    I(lag(cso, 1:2)) +
    I(lag(t.bal, 1:2)),
  qoi = "att",
  lead = 0:4,
  outcome.var = "w.civ.lib",
  restrict.control.period = 2
)

civ.psw.3 <- PanelMatch(
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

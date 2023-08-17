############################################################################
#################--------------Data Collection-------------#################
############################################################################

library(dplyr)
library(readxl)

##### Load V-Dem Data ######
vdem <- read_excel("Data/Selected VDEM Data.xlsx")

# Rename Variables
vdem <- vdem %>%
  rename(
    ccode = COWcode,
    dem = v2x_polyarchy,
    h.court = v2juhcind,
    l.court = v2juncind,
    pop = e_pop,
    gdppc = e_gdppc,
    exports = e_cow_exports,
    imports = e_cow_imports,
    polity = e_polity2,
    p.vio = v2x_clphy,
    gen.exc = v2xpe_exlgender,
    w.pol.emp = v2x_gender,
    w.civ.lib = v2x_gencl,
    torture = v2cltort,
    cso = v2csprtcpt
  )

# Manipulate Data
vdem <- vdem %>%
  # Transpose Data with Negative Values
  mutate(h.court = h.court + 3.405) %>%
  mutate(l.court = l.court + 3.37) %>%
  mutate(polity = if_else(polity %in% c(-88, -77, -66), NA_real_, polity)) %>%
  mutate(polity = polity + 10) %>%
  mutate(torture = torture + 3.278) %>%
  mutate(cso = cso + 3.36) %>%
  # Create Log Transformations
  mutate(lgdppc = log(gdppc + 1)) %>%
  mutate(lpop = log(pop + 1)) %>%
  mutate(lexports = log(exports + 1)) %>%
  mutate(limports = log(imports + 1)) %>%
  # Drop Unlogged Variables
  select(-c(gdppc, pop, exports, imports))

##### Load COW Data #####
nmc <- read.csv("Data/NMC-60-abridged.csv")

nmc <- nmc %>%
  select(ccode, year, milper) %>%
  mutate(milper = replace(milper, milper == -9, NA)) %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(-c(milper))

### Merge the Data Sets
final <- full_join(vdem, nmc, by = c("ccode", "year"))

##### Load UCDP Interstate Data #####
ucdp.inter <- read_excel("Data/ucdp.23.1.inter.xlsx")

ucdp.inter <- ucdp.inter %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(type_of_conflict == 2) %>%
  mutate(inter.war = 1) %>%
  group_by(gwno_a, year) %>%
  summarise(inter.war = max(inter.war)) %>%
  ungroup()

### Merge This Data with COW System Data to Identify All Country-Years
cow <- read.csv("Data/cow.states.csv")

ucdp.inter <- left_join(cow, ucdp.inter,
                  by = c("ccode" = "gwno_a", "year"))

# Manipulate Data
ucdp.inter <- ucdp.inter %>%
  # Replace NAs with 0
  mutate(inter.war = if_else(is.na(inter.war), 0, inter.war)) %>%
  # Only Keep Desired Variables
  select(ccode, year, inter.war)

### Merge with Final Data
final <- left_join(final, ucdp.inter,
                   by = c("ccode", "year"))

##### Load UCDP Intrastate Data #####
ucdp.intra <- read_excel("Data/ucdp.23.1.intra.xlsx")

ucdp.intra <- ucdp.intra %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(type_of_conflict == 3) %>%
  mutate(intra.war = 1) %>%
  group_by(gwno_a, year) %>%
  summarise(intra.war = max(intra.war)) %>%
  ungroup()

### Merge This Data with COW System Data to Identify All Country-Years
ucdp.intra <- left_join(cow, ucdp.intra,
                        by = c("ccode" = "gwno_a", "year"))

# Manipulate Data
ucdp.intra <- ucdp.intra %>%
  # Replace NAs with 0
  mutate(intra.war = if_else(is.na(intra.war), 0, intra.war)) %>%
  # Only Keep Desired Variables
  select(ccode, year, intra.war)

### Merge with Final Data
final <- left_join(final, ucdp.intra,
                   by = c("ccode", "year"))

# Make Pre-1946 War Observations NAs
final <- final %>%
  mutate(inter.war = if_else(year < 1946, NA_real_, inter.war)) %>%
  mutate(intra.war = if_else(year < 1946, NA_real_, intra.war))


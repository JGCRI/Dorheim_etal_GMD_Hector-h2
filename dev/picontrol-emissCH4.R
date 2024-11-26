# Objective: Run Hector with the emission driven CH4 runs, these will be used to
# make sure that the model dev are having the effects we expect them to.

# 0. Set Up --------------------------------------------------------------------
# TODO load the correct version of Hector to use, until the h2 dev is merged into
# main pull you will want to use the h2 dev branch.
#remotes::install_github("jgcri/hector@dev-h2")
#library(hector)
# This is where KD is actively devloping the H2 capabilites on her machince,
# it will not be useful for others.
devtools::load_all("/Users/dorh012/Documents/2024/H2Materials/hector")

library(dplyr)
library(ggplot2)

theme_set(theme_bw())

dates <- 1800:2100
vars <- c(GLOBAL_TAS(), RF_CH4(), EMISSIONS_CH4(), EMISSIONS_CO(),
          CONCENTRATIONS_CH4(), LIFETIME_OH(), EMISSIONS_H2(), EMISSIONS_NOX(),
          EMISSIONS_CO(), EMISSIONS_NMVOC())

DATA_DIR <- here::here("dev", "data")

# Read in the default data that will be used in comparisons.
default_rslts <- read.csv(file.path(DATA_DIR, "default_irf.csv"))

# 1. Hector Runs ---------------------------------------------------------------

# The default run
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "pi control")
run(core)
out1 <- fetchvars(core, dates, vars)
shutdown(core)


# Impulse of CH4 emissions
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "ch4 impulse")
ECH4_1850 <- fetchvars(core, 1745, EMISSIONS_CH4())
setvar(core, 1850, var = EMISSIONS_CH4(), values = ECH4_1850$value * 2,
       unit = getunits(EMISSIONS_CH4()))
reset(core)
run(core)
out2 <- fetchvars(core, dates, vars)
shutdown(core)

# Impulse of CO emissions
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "co impulse")
ECO_1850 <- fetchvars(core, 1745, EMISSIONS_CO())
setvar(core, 1850, var =  EMISSIONS_CO(), values = ECO_1850$value * 2,
       unit = getunits(EMISSIONS_CO()))
reset(core)
run(core)
out3 <- fetchvars(core, dates, vars)
shutdown(core)

# Impulse of NOx emissions
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "nox impulse")
NOx_1850 <- fetchvars(core, 1745, EMISSIONS_NOX())
setvar(core, 1850, var =  EMISSIONS_NOX(), values = NOx_1850$value * 2,
       unit = getunits(EMISSIONS_NOX()))
reset(core)
run(core)
out4 <- fetchvars(core, dates, vars)
shutdown(core)


# Impulse of NMVOC emissions
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "nmvoc impulse")
NMVOC_1850 <- fetchvars(core, 1745, EMISSIONS_NMVOC())
setvar(core, 1850, var =  EMISSIONS_NMVOC(), values = NMVOC_1850$value * 2,
       unit = getunits(EMISSIONS_NMVOC()))
reset(core)
run(core)
out5 <- fetchvars(core, dates, vars)
shutdown(core)


# 2. Plot Results --------------------------------------------------------------
# TODO it might be good to compare the results with the default results.
out <- rbind(out1, out2, out3, out4, out5)

ggplot(out, aes(year, value, color = scenario)) +
    geom_line(size = 1) +
    facet_wrap("variable", scales = "free")

# Objective: Run Hector with the emission driven CH4 runs, these will be used to
# make sure that the model dev are having the effects we expect them to, right now
# save a copy of the "default" hector aka before any of the H2 interaction terms
# have been implement. I suspect that this will help us understand potential
# interactions during the dev phase. Here we do impulses of all the following
# emissions CH4, NOx, CO, NMVOC

# 0. Set Up --------------------------------------------------------------------
# Install a specific version of Hector aka the Hector that had tau OH as a potential
# output but otherwise is default or main.
tag <- "1e15620"
remotes::install_github(paste0("jgcri/hector@", tag))
library(hector)
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

dates <- 1800:2100
vars <- c(GLOBAL_TAS(), RF_CH4(), EMISSIONS_CH4(), EMISSIONS_CO(),
          CONCENTRATIONS_CH4(), LIFETIME_OH(), EMISSIONS_H2(), EMISSIONS_NOX(),
          EMISSIONS_CO(), EMISSIONS_NMVOC())

DATA_DIR <- here::here("dev", "data")


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

<<<<<<< HEAD
# 2. Calculate CH4 lifetime ----------------------------------------------------

ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini)
CH4_PREIND <- fetchvars(core, NA, PREINDUSTRIAL_CH4())[["value"]]


# Let's take a look at the CH4 impulse
out2 %>%
    filter(variable == CONCENTRATIONS_CH4()) %>%
    # Change to relative since the pulse
    mutate(year = year - 1850) %>%
    filter(year >= 0) %>%
    mutate(value = value - CH4_PREIND) %>%
    select(time = year, C = value) ->
    ch4_rslts


ch4_rslts  %>%
    ggplot(aes(time, C)) +
    geom_line(size = 1)


C0 <- max(ch4_rslts$C)

# Fit the model: C(t) = C0 * exp(-t / tau)
model <- nls(C ~ C0 * exp(-time / tau),
             start = list(C0 = C0, tau = 1), data = ch4_rslts)

# Extract tau
tau <- coef(model)["tau"]; tau
# tau
# 8.088113

# Looking into the AR6
# https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter06.pdf


# 3. Plot Results --------------------------------------------------------------

out <- rbind(out1, out2, out3, out4, out5) %>%
    filter(scenario != "ch4 impulse")

ggplot(out, aes(year, value, color = scenario)) +
    geom_line(size = 1) +
    facet_wrap("variable", scales = "free")

# 3. Save Results --------------------------------------------------------------
# Save some information about which version of Hector this came from, including
# both the name and git tag might be over kill alas.
out$source <- "default"
out$git <- tag
write.csv(out, file = file.path(DATA_DIR, "default_irf.csv"), row.names = FALSE)


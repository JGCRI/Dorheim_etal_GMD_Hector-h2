# Description: GWP 100  calculations for H2! Figures and results will be generated
# and written out to disk.

# 0. Set Up --------------------------------------------------------------------
source("scripts/0.env_fxns.R")

RSLT_DATES <- 1750:2100
RSLT_VARS <- c(RF_CO2(), RF_TOTAL(), GLOBAL_TAS(),
               CH4_LIFETIME_OH(), RF_O3_TROP(),
               FFI_EMISSIONS(), RF_H2O_STRAT(),
               RF_CH4(), "TAU_OH", "rh_ch4",
               CONCENTRATIONS_CO2(), GMST(), EMISSIONS_H2(),
               CONCENTRATIONS_CH4())

# Define the impulse year
IMPULSE_YR <- 1850


# 1. CO2 GWP ------------------------------------------------------------------
# Set up the emission driven pi control run and use the constant background
# of 378 ppm, used in AR4(Forster et al., 2007; Section 2.10.2)[https://archive.ipcc.ch/publications_and_data/ar4/wg1/en/ch2s2-10-2.html]
ini <- here("inputs", "hector_CO2esm-picontrol.ini")
hc <- newcore(ini)
setvar(core = hc, dates = NA, var = PREINDUSTRIAL_CO2(),
       values = 378, unit = getunits(PREINDUSTRIAL_CO2()))
reset(hc)
run(hc)

# Extract the control run results.
fetchvars(hc, RSLT_DATES, RSLT_VARS) %>%
    rename(control = value) %>%
    distinct ->
    control_out

# Extract the FFI emissions, in theory it should be 0 but let's doubble check.
control_out %>%
    filter(variable == FFI_EMISSIONS() & year == IMPULSE_YR) %>%
    pull(control) ->
    contorl_emiss_val
stopifnot(contorl_emiss_val == 0)

# Now we want to do a pulse of CO2 equivalent to 1 Tg of CO2 but Hector inputs are in
# Pg C so need to convert before adding the pulse to the FFI EMISSIONS.
CO2_Tg <- 1
CONVERSION_FACTOR <- (12.01/44.01) * 1e-3 # convert from Tg CO2 to PgC
impulse <- contorl_emiss_val + (CO2_Tg * CONVERSION_FACTOR)

# Set the impulse emissions and run hector.
setvar(hc, IMPULSE_YR, FFI_EMISSIONS(), impulse, getunits(FFI_EMISSIONS()))
reset(hc)
run(hc)

# Extract the impulse run results
fetchvars(hc, RSLT_DATES, RSLT_VARS) %>%
    rename(impulse = value) %>%
    distinct ->
    impulse_out

# Clean up the hector core.
shutdown(hc)

# Because of the way that Hector's carbon cycle runs offset the impulse year by 1
control_out %>%
    left_join(impulse_out) %>%
    mutate(value = impulse - control) %>%
    mutate(year = year - (IMPULSE_YR+1)) %>%
    filter(year >= 0) %>%
    filter(year <= 100) %>%
    select(year, variable, value, units) %>%
    mutate(run = "co2") ->
    co2_100_rslts

co2_100_rslts %>%
    filter(variable == RF_TOTAL()) %>%
    filter(year >= 0 & year <= 100) %>%
    summarise(co2 = sum(value), .by = "variable") %>%
    pull(co2) ->
    CO2_GWP100

# Note: GWP100 of CO2 is 0.0895 10− 12 Wm− 2 kg− 1 yr from IPCC AR6 Table 7.SM.648
# Which is different than hectors.

# 2. H2 GWP -------------------------------------------------------------------
# As to be consistent with Sand et al. "since hydrogen does not have a direct
# radiate forcing, but it has various indirect forcing effects
# (i.e., methane, ozone, and stratospheric water vapor). These forcing terms are summed"

# Again to be consistent we will use the constant background of 378 ppm [CO2].
ini <- here("inputs", "hector_CH4esm-picontrol.ini")
hc <- newcore(ini)
setvar(core = hc, dates = NA, var = PREINDUSTRIAL_CO2(),
       values = 378, unit = getunits(PREINDUSTRIAL_CO2()))
reset(hc)
run(hc)

# Extract the control run results.
fetchvars(hc, RSLT_DATES, RSLT_VARS) %>%
    rename(control = value) %>%
    distinct ->
    control_out

# Extract the H2 emissions, in theory it should be 0 but let's double check.
control_out %>%
    filter(variable == EMISSIONS_H2() & year == IMPULSE_YR) %>%
    pull(control) ->
    contorl_emiss_val
stopifnot(contorl_emiss_val == 0)

# Define the H2 emissions impulse size
H2_PULSE <- 1

# Set the impulse emissions and run hector.
setvar(hc, IMPULSE_YR, EMISSIONS_H2(), H2_PULSE, getunits(EMISSIONS_H2()))
reset(hc)
run(hc)

# Extract the impulse run results
fetchvars(hc, RSLT_DATES, RSLT_VARS) %>%
    rename(impulse = value) %>%
    distinct ->
    impulse_out

# Extract the impulse response results
control_out %>%
    left_join(impulse_out) %>%
    mutate(value = impulse - control) %>%
    mutate(year = year - IMPULSE_YR) %>%
    filter(year >= 0) %>%
    filter(year <= 100) %>%
    select(year, variable, value, units) %>%
    filter(variable %in% c(RF_CO2(), RF_TOTAL(), RF_CH4(), RF_H2O_STRAT(), RF_O3_TROP())) %>%
    summarise(value = sum(value), .by = variable) ->
    GWPs_m

# 3. Hector H2 AGWP ------------------------------------------------------------
# Optional what when we take a look at the AGWP when we use the AR6 GWPCO2?
# CO2_GWP100 <- 8.95e-5
# When this happens we are consistent with the values... Perhaps there is
# a different set of results that we would want to show? Also this does not
# include the carbon cycle feedback interactions....

GWPs_m %>%
    mutate(CO2 = CO2_GWP100) %>%
    mutate(GWP = value / CO2) %>%
    mutate(variable = if_else(variable == "RF_H2O_strat", "strat_H2O", variable)) %>%
    mutate(variable = if_else(variable == "RF_O3_trop", "O3", variable)) %>%
    mutate(variable = if_else(variable == "RF_CH4", "CH4", variable)) %>%
    mutate(variable = if_else(variable == RF_TOTAL(), "Total", variable)) %>%
    select(variable, GWP) ->
    hector_GWP_100

hector_GWP_100 %>%
    filter(variable == "Total") %>%
    pull(GWP) ->
    hector_total_GWP

hector_GWP_100 %>%
    filter(variable %in% c("O3", "strat_H2O", "CH4")) %>%
    mutate(percent = signif(x = 100 * GWP/hector_total_GWP, 3)) %>%
    mutate(GWP = signif(GWP, 3)) ->
    GWP_by_percent

# Write the results mentioned in the manuscript text to file
write_results(name = "Hector’s total GWP100  is", val = hector_total_GWP)
write_results(name = "contribution to GWP100 by %", val = GWP_by_percent)

# 4.  Hector H2 AGWP Figure  ---------------------------------------------------
# Read in the benchmarking data.
read.csv(file = file.path("data", "Sand_SITable6.csv"),
         comment.char = "#") %>%
    tidyr::pivot_longer(-Name, names_to = "variable", values_to = "GWP") ->
    sand_GWP

sand_GWP %>%
    filter(Name != "MM") ->
    sand_GWP_models

sand_GWP %>%
    filter(Name == "MM") ->
    sand_GWP_MM


hector_GWP_100 %>%
    filter(variable %in% c("CH4", "O3", "strat_H2O", "Total")) ->
    GWP100_var

ggplot() +
    geom_point(data = sand_GWP_models, aes(variable, GWP, color = "Sand et al."),
               position = position_jitter(height = 0, seed = 42, width = 0.1),
               alpha = 0.5, shape = 20) +
    geom_point(data = sand_GWP_MM, aes(variable, GWP, color = "Sand et al."), size = 3, shape = 19) +
    geom_point(data = GWP100_var, aes(variable, GWP, color = "Hector"), size = 4, shape = 18, alpha = 1) +
    theme(legend.title =  element_blank()) +
    labs(y = "GWP100 H2", x = NULL) +
    scale_color_manual(values = c("Sand et al." = "black", "Hector" = "red")) ->
    plot; plot

custom_ggsave(p = plot, DIR = DIRS$RSLTS, name = "Fig_2", WIDTH = 5, HEIGHT = 3.3)




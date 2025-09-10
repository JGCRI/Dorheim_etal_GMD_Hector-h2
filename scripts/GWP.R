# Description: GWP 20 and 100  calculations for H2!

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")


RSLT_DATES <- 1750:2100
RSLT_VARS <- c(RF_CO2(), RF_TOTAL(), GLOBAL_TAS(),
               CH4_LIFETIME_OH(), RF_O3_TROP(),
               FFI_EMISSIONS(), RF_H2O_STRAT(), RF_CH4(), "TAU_OH", "rh_ch4")
dates <- RSLT_DATES

# 1. CO2 Portion ---------------------------------------------------------------
# For the CO2 portion of the GWP we will use a mulitforcing scenario as the
# control since we do not have a CO2 emission driven PI control

# Our control run
inifile <- file.path(HECTOR_DIR, "inst", "input", "hector_ssp245.ini")
hc <- newcore(inifile, name = "control")
run(hc)
fetchvars(hc, dates, RSLT_VARS) %>%
    rename(control = value) ->
    out1
shutdown(hc)

# Impulse run
IMPULSE_YR <- 1850
inifile <- file.path(HECTOR_DIR, "inst", "input", "hector_ssp245.ini")
hc <- newcore(inifile, name = "impulse")
run(hc)
contorl_emiss_val <- fetchvars(hc, IMPULSE_YR, FFI_EMISSIONS())$value
# We want our pulse to only equivalent to 1 Tg of CO2 but Hector inputs are in
# Pg C so need to convert before adding the pulse to
CO2_Tg <- 1
CONVERSION_FACTOR <- (12.01/44.01) * 1e-3 # convert from Tg CO2 to PgC
impulse <- contorl_emiss_val + (CO2_Tg * CONVERSION_FACTOR)
setvar(hc, IMPULSE_YR, FFI_EMISSIONS(), impulse, getunits(FFI_EMISSIONS()))
reset(hc)
run(hc)
fetchvars(hc, dates, RSLT_VARS) %>%
    rename(impulse = value) ->
    out2
shutdown(hc)

# Because of the way that Hector's carbon cycle runs offset the impulse year by 1
out1 %>%
    cbind(impulse = out2$impulse) %>%
    mutate(value = impulse - control) %>%
    mutate(year = year - (IMPULSE_YR+1)) %>%
    filter(year >= 0) %>%
    filter(year <= 100) %>%
    select(year, variable, value, units) %>%
    mutate(run = "co2") ->
    co2_100_rslts

co2_100_rslts %>%
    filter(variable == RF_TOTAL()) %>%
    ggplot() +
    geom_line(aes(year, value))


# 2. H2 Runs  ---------------------------------------------------------------

# TODO - consider using a mmultiforcing run?

# The full GWP
ini_file <- "inputs/hector_picontrol.ini"
hc <- newcore(ini = ini_file)


# Helper function that runs the H2 impulse
# Args
#   hc: active hector core (with parameters set)
#   name: the run name
# returns: data frame of the impulse response to an impulse of H2 emissions
get_h2_irf <- function(hc, name){

    # Reference
    run(hc)
    ref <- fetchvars(hc, dates, RSLT_VARS)

    # Impulse 1
    H2_PULSE <- 1
    IMPULSE_YR <- 1850
    setvar(hc, IMPULSE_YR,
           var =  EMISSIONS_H2(),
           values = H2_PULSE,
           unit = getunits(EMISSIONS_H2()))
    reset(hc)
    run(hc)

    # Format output
    fetchvars(hc, dates, RSLT_VARS) %>%
        # Remove the reference
        mutate(value = value - ref$value) %>%
        mutate(year = year - IMPULSE_YR) %>%
        select(year, variable, value, units) %>%
        mutate(name = name) ->
        out

    return(out)

}



# with all the indirect climate effects
ini_file <- "inputs/hector_picontrol.ini"
hc <- newcore(ini = ini_file)
out1 <- get_h2_irf(hc, name = "Total")


out1 %>%
    filter(variable == RF_CO2()) %>%
    ggplot(aes(year, value)) +
    geom_line()


# only the strat h2o
ini_file <- "inputs/hector_picontrol.ini"
hc <- newcore(ini = ini_file)
setvar(hc, NA, "H2_CCO", 0, "(undefined)")
reset(hc)
setvar(hc, NA, "CO3_H2", 0, "(undefined)")
reset(hc)

# with all the indirect climate effects
out2 <- get_h2_irf(hc, name = "strat_H2O")


# only the o3
ini_file <- "inputs/hector_picontrol.ini"
hc <- newcore(ini = ini_file)
setvar(hc, NA, "H2_CCO", 0, "(undefined)")
reset(hc)
setvar(hc, NA, "rho_h2o_h2", 0, "(undefined)")
reset(hc)

# with all the indirect climate effects
out3 <- get_h2_irf(hc, name = "O3")


# only the ch4 only
ini_file <- "inputs/hector_picontrol.ini"
hc <- newcore(ini = ini_file)
setvar(hc, NA, "CO3_H2", 0, "(undefined)")
reset(hc)
setvar(hc, NA, "rho_h2o_h2", 0, "(undefined)")
reset(hc)

# with all the indirect climate effects
out4 <- get_h2_irf(hc, name = "CH4")


# 3. GWP  ---------------------------------------------------------------
# Integrate both the H2 and CO2 impulse response functions
rbind(out1, out2, out3, out4) %>%
    filter(year >= 0 & year <= 100) %>%
    summarise(h2_value = sum(value), .by = c("name", "variable")) ->
    h2_integral

co2_100_rslts %>%
    filter(year >= 0 & year <= 100) %>%
    summarise(co2 = sum(value), .by = "variable") ->
    co2_integral

# Calculate GWP 100 (the ratio of the RF total)
h2_integral %>%
    full_join(co2_integral, relationship = "many-to-many") %>%
    filter(variable == "RF_tot") %>%
    mutate(GWP =round(h2_value/ co2, 3)) %>%
    select(variable = name, GWP) %>%
    mutate(Name = "hector") ->
    GWP100

# What if we did the GWP by RF components only by the specific RF?
# Calculate GWP 100 (the ratio of the RF total)
co2_100_rslts %>%
    filter(variable == RF_CO2()) %>%
    filter(year >= 0 & year <= 100) %>%
    summarise(co2 = sum(value), .by = "variable") %>%
    mutate(name = "Total") %>%
    select(-variable)->
    co2_integral_co2


h2_integral %>%
    filter(name == "Total") %>%
    filter(grepl(pattern = "RF|", variable)) %>%
    full_join(co2_integral_co2, relationship = "many-to-many") %>%
    mutate(GWP =round(h2_value/ co2, 3)) %>%
    select(name, GWP, variable) %>%
    mutate(Name = "hector") %>%
    mutate(variable = if_else(variable == RF_TOTAL(), "Total", variable)) %>%
    mutate(variable = if_else(variable == "RF_H2O_strat", "strat_H2O", variable)) %>%
    mutate(variable = if_else(variable == "RF_O3_trop", "O3", variable)) %>%
    mutate(variable = if_else(variable == "RF_CH4", "CH4", variable)) ->
    GWP100_var




# 4. Figures  ------------------------------------------------------------------


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


setdiff(sand_GWP_models$variable, GWP100$variable)
setdiff( GWP100$variable, sand_GWP_models$variable)

GWP100_var %>%
    filter(variable %in% c("CH4", "O3", "strat_H2O", "Total")) ->
    GWP100_var

ggplot() +
    geom_point(data = sand_GWP_models, aes(variable, GWP),
               position = position_jitter(height = NULL, seed = 42, width = 0.1),
               alpha = 0.5, size = 3, shape = 20) +
    geom_point(data = sand_GWP_MM, aes(variable, GWP), size = 3, shape = 19) +
   # geom_point(data = GWP100, aes(variable, GWP, color = "hector"), size = 6, shape = 18) +
    geom_point(data = GWP100_var, aes(variable, GWP, color = "hector"), size = 6, shape = 18, alpha = 0.8) +
    theme(legend.title = element_blank()) +
    labs(y = "GWP100 H2", x = NULL)






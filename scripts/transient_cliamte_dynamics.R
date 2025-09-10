# Description: Hector's response to an impulse of H2 emissions.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")


dates <- 1745:2100
vars <- c(GLOBAL_TAS(), RF_CH4(), EMISSIONS_CH4(), EMISSIONS_CO(),
          CONCENTRATIONS_CH4(), "TAU_OH", EMISSIONS_H2(), EMISSIONS_NOX(),
          EMISSIONS_CO(), EMISSIONS_NMVOC(), RF_H2O_STRAT(), RF_O3_TROP(), RF_CH4(), RF_TOTAL(),
          NATURAL_CH4(),
          CONCENTRATIONS_CO2())


# H2 Impulse emissions
ini <- "inputs/hector_picontrol.ini"
core <- newcore(ini, name = "h2 impulse")
run(core)

# Right now emissions are set to 0, it is unclear what values we should use.
vals <- rep(0, length(dates))
setvar(core, dates,
       var =  EMISSIONS_H2(),
       values = vals,
       unit = getunits(EMISSIONS_H2()))
reset(core)

H2_PULSE <- 500
PULSE_YR <- 1850
setvar(core,
       dates = PULSE_YR,
       var =  EMISSIONS_H2(),
       values = H2_PULSE,
       unit = getunits(EMISSIONS_H2()))
reset(core)
run(core)


fetchvars(core, dates, vars) %>%
    mutate(year = year - PULSE_YR) %>%
    filter(year >= -10) ->
    impulse_results


VARS <- c(RF_TOTAL(), RF_CH4())

impulse_results %>%
    filter(variable %in% VARS) %>%
    filter(year <= 40) %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line()




# 2. Plot Results --------------------------------------------------------------
# TODO it might be good to compare the results with the default results.
out <- rbind(out1, out2)

rf_temp_vars <- c("RF_CH4 (W/m2)", "RF_H2O_strat (W/m2)", "RF_O3_trop (W/m2)", "global_tas (degC)")


out2 %>%
    mutate(variable_units = paste0(variable, " (", units, ")")) %>%
  #  filter(variable_units %in% rf_temp_vars) %>%
    mutate(year = year - 1850) %>%
    filter(year >= -10 & year <= 100) %>%
    filter(year <= 50) ->
    out


out %>%
    filter(variable %in% c(RF_CH4(), RF_O3_TROP(), RF_H2O_STRAT(), RF_TOTAL())) %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line()


out %>%
    filter(variable == GLOBAL_TAS()) %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line()



out$variable <- factor(out$variable, levels = rf_temp_vars)


ggplot(out, aes(year, value, color = scenario)) +
    geom_line(linewidth = 1) +
    facet_wrap("variable", scales = "free") +
    labs(y = NULL, x = NULL) +
    theme_bw(base_size = 16) +
    facet_wrap("variable", scales = "free") +
    theme(legend.position = "none") ->
    plot; plot







ggsave(plot = plot, filename = "AGU/impuse1.pdf")

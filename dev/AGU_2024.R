# Objective: Run Hector with the emission driven CH4 runs, these will be used to
# make sure that the model dev are having the effects we expect them to, this
# specific version uses the coefficent calcualted by the sponsor.

# 0. Set Up --------------------------------------------------------------------
# TODO load the correct version of Hector to use, until the h2 dev is merged into
# main pull you will want to use the h2 dev branch.
#remotes::install_github("jgcri/hector@dev-h2")
#library(hector)
# This is where KD is actively developing the H2 capabilities on her machine,
# it will not be useful for others.
devtools::load_all("/Users/dorh012/Documents/2024/H2Materials/hector")

# hector git tag




library(dplyr)
library(ggplot2)

theme_set(theme_bw())

dates <- 1800:2100
vars <- c(GLOBAL_TAS(), RF_CH4(), EMISSIONS_CH4(), EMISSIONS_CO(),
          CONCENTRATIONS_CH4(), LIFETIME_OH(), EMISSIONS_H2(), EMISSIONS_NOX(),
          EMISSIONS_CO(), EMISSIONS_NMVOC(), RF_H2O_STRAT(), RF_O3_TROP(), RF_CH4(), RF_TOTAL())

DATA_DIR <- here::here("dev", "data")

# 1. Hector Runs ---------------------------------------------------------------

# The default run
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "pi control")
run(core)
out1 <- fetchvars(core, dates, vars)
shutdown(core)


# H2 Impulse emissions
ini <- "inputs/picontrol_ch4-emiss.ini"
core <- newcore(ini, name = "h2 impulse")
# Right now emissions are set to 0, it is unclear what values we should use.
H2_PULSE <- 400
setvar(core, 1850, var =  EMISSIONS_H2(), values = H2_PULSE,
       unit = getunits(EMISSIONS_H2()))
reset(core)
run(core)
out2 <- fetchvars(core, dates, vars)
shutdown(core)

# 2. Plot Results --------------------------------------------------------------
# TODO it might be good to compare the results with the default results.
out <- rbind(out1, out2)

out <- out2 %>%
    filter(variable %in% c("TAU_OH", CONCENTRATIONS_CH4(), RF_CH4(),
                           RF_H2O_STRAT(), RF_TOTAL(), GLOBAL_TAS())) %>%
    mutate(variable = paste0(variable, " (", units, ")")) %>%
    mutate(year = year - 1850) %>%
    filter(year >= -2 & year <= 100) %>%
    filter(year <= 50)

out$variable <- factor(out$variable, levels =  c("TAU_OH (Years)", "CH4_concentration (ppbv CH4)", "FCH4 (W/m2)",
                                                 "RF_H2O_strat (W/m2)", "RF_tot (W/m2)", "global_tas (degC)"))


ggplot(out, aes(year, value, color = scenario)) +
    geom_line(linewidth = 1) +
    facet_wrap("variable", scales = "free") +
    labs(y = NULL, x = NULL) +
    theme_bw(base_size = 16) +
    facet_wrap("variable", scales = "free") +
    theme(legend.position = "none") ->
    plot; plot

ggsave(plot = plot, filename = "AGU/impuse1.pdf")


"inputs/Figure_Data-H2_emiss-Comb-Total.csv" %>%
    read.csv() %>%
    ggplot(aes(year, H2_emissions)) +
    geom_line(size = 1.5) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 20) ->
    plot; plot

ggsave(plot = plot, filename = "AGU/h2_emiss.pdf")




# Quickly run Hector with

ini <- "inputs/hector_ssp245-H20.ini"
core <- newcore(ini, name = "default")
run(core)
out1 <- fetchvars(core, dates, vars)
shutdown(core)


ini <- "inputs/hector_ssp245-H2-hist.ini"
core <- newcore(ini, name = "hist")
run(core)
out2 <- fetchvars(core, dates, vars)
shutdown(core)


out <- rbind(out1, out2)


out %>%
    filter(year <= 2022) %>%
    filter(variable %in% c(GLOBAL_TAS(), CONCENTRATIONS_CH4())) ->
    df

df %>%
    ggplot(aes(year, value, color = scenario, linetype = scenario)) +
    geom_line(size = 1.5) +
    facet_wrap("variable", scales = "free") +
    scale_color_manual(values = c("default" = "black", "hist" = "red")) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 20) ->
   # theme(legend.position = "non") ->
    plot

ggsave(plot = plot, filename = "AGU/historial_rslts.pdf")






df %>%
    select(scenario, year, variable, value) %>%
    tidyr::pivot_wider(names_from = "scenario") %>%
    mutate(dif = 100 * (hist - default)/default) %>%
    summarise(mean_dif = mean(dif), .by = variable)





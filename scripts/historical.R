# Description: the historical stand alone hector run.

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")


# Convert from kt to Tg of H2
Kt_Tg <- 0.001

# Load P. ORourke historical H2 emissions inventory.
"data/D.H2_emissions_global_total.csv" %>%
    read.csv %>%
    pivot_longer(values_to = "value", cols = starts_with("X")) %>%
    mutate(year = as.integer(gsub(x = name, replacement = "", pattern = "X"))) %>%
    select(value, year) %>%
    mutate(value = Kt_Tg * value) %>%
    mutate(units = getunits(EMISSIONS_H2()),
           variable = EMISSIONS_H2()) ->
    hist_h2_inputs

hist_h2_inputs %>%
    filter(year %in% 1750:1755) %>%
    pull(value) %>%
    mean ->
    PI_mean


data.frame(year = 1745:1749,
           value = PI_mean,
           units = getunits(EMISSIONS_H2()),
           variable = EMISSIONS_H2()) %>%
    rbind(hist_h2_inputs) ->
    hist_h2_inputs


write.csv(hist_h2_inputs, file = "data/hist_h2_inputs.csv", row.names = FALSE)

# Dates to save
RSLT_DATES <- 1750:2022

# Variables to save
RSLT_VARS <- c("RF_CO2", "RF_tot", "global_tas", "TAU_OH",  "RF_O3_trop", RF_CH4(),
               "ffi_emissions", "RF_H2O_strat",  "TAU_OH", "rh_ch4", CONCENTRATIONS_CH4(),
               CONCENTRATIONS_CO2(), CONCENTRATIONS_O3())


# 1. Reference Scenario --------------------------------------------------------

ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc  <- newcore(ini)
run(hc, runtodate = max(RSLT_DATES))
out1 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
out1$scenario <- "default"

# 2. With H2 Emissions ---------------------------------------------------------
# now run with h2 emissions
setvar(hc, dates = hist_h2_inputs$year, var = hist_h2_inputs$variable,
       values = hist_h2_inputs$value, unit = hist_h2_inputs$units)
reset(hc)
run(hc)

out2 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
out2$scenario <- "h2 effects"


# Plot the H2 Emissions Inventory
ggplot() +
    geom_line(data = hist_h2_inputs, aes(year, value)) +
    labs(y = "Tg",
         x = NULL) +
    theme(legend.title = element_blank()) ->
    plot; plot

custom_ggsave(p = plot, DIR = FIGS_DIR, name = "hist_H2", WIDTH = 8, HEIGHT = 4)

# 3. RF Plot -------------------------------------------------------------------

VAR <- RF_TOTAL()
UNITS <- unique(out1[out1$variable == VAR,]$units)


rbind(out1, out2) %>%
    filter(variable == VAR) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() +
    labs(title = VAR,
         y = UNITS,
         x = NULL) ->
    total_RF


rbind(out1, out2) %>%
    filter(year >= 1850) %>%
    distinct() %>%
    filter(grepl(pattern = "RF", x = variable)) %>%
    spread(scenario, value) %>%
    mutate(diff = `h2 effects` - default) ->
    diff_df

diff_df   %>%
    filter(variable != RF_TOTAL()) ->
    componets

diff_df   %>%
    filter(variable == RF_TOTAL()) %>%
    # Change the variable name for nice plot labels.
    mutate(variable = "Total ERF") ->
    total


ggplot() +
    geom_area(data = componets, aes(year, diff, fill = variable)) +
    geom_line(data = total, aes(year, diff, color = variable), linewidth = 1) +
    labs(y = expression(Delta~ "W/m2"),
         x = NULL) +
    scale_color_manual(values = c("Total ERF" = "black")) +
    theme(legend.title = element_blank()) ->
    plot; plot

custom_ggsave(p = plot, DIR = FIGS_DIR, name = "histERF", WIDTH = 8, HEIGHT = 4)


rbind(out1, out2) %>%
    distinct() %>%
    spread(scenario, value) %>%
    mutate(AE = abs(`h2 effects` - default)) %>%
    summarise(MAE = mean(AE), .by = "variable") %>%
    filter(variable %in% c("RF_tot", "global_tas")) %>%
    mutate(MAE = signif(MAE, digits = 2)) ->
    historcal_changes

write_results(name = "Historical MAE", val = historcal_changes)


componets %>%
    select(year, variable, num = diff) %>%
    left_join(total %>% select(year, tot = diff)) %>%
    mutate(percent = 100 * (num/tot)) %>%
    summarise(percent = mean(percent), .by = variable) %>%
    mutate(percent = signif(percent, digits = 3)) ->
    mean_percent_rf; mean_percent_rf

write_results(name = "Historical RF % Change by componet", val = mean_percent_rf)


mean_percent_rf %>%
    filter(variable != RF_CO2()) %>%
    pull(percent) %>% sum()



# 4. Concentrations Change -----------------------------------------------------
#  "ppbv CH4" "ppmv CO2" "DU O3"

rbind(out1, out2) %>%
    filter(year %in% 1850:2015) %>%
    distinct() %>%
    filter(variable %in% c(CONCENTRATIONS_CH4(), CONCENTRATIONS_CO2(), CONCENTRATIONS_O3())) %>%
    spread(scenario, value) %>%
    mutate(diff = 100 * (`h2 effects` - default)/default) ->
    diff_df

diff_df$year %>% range()

diff_df %>%
    summarise(min  = min(diff),
              max = max(diff),
              mean = mean(diff), .by = "variable")



diff_df %>%
    ggplot(aes(year, diff, color = variable)) +
    geom_line(linewidth = 1) +
    theme(legend.title = element_blank()) +
    labs(x = "Year", y = "% Difference") ->
    plot; plot

custom_ggsave(p = plot, DIR = FIGS_DIR, name = "delta_hist_conc", WIDTH = 8, HEIGHT = 4)


# 5. Temperature Change -----------------------------------------------------
rbind(out1, out2) %>%
    filter(year %in% 1850:2015) %>%
    distinct() %>%
    filter(variable %in% GLOBAL_TAS()) %>%
    spread(scenario, value) %>%
    mutate(diff = 100 * (`h2 effects` - default)/default) ->
    diff_df

diff_df %>%
    ggplot(aes(year, diff, color = variable)) +
    geom_line(linewidth = 1) +
    theme(legend.title = element_blank()) +
    labs(x = "Year", y = "% Difference") ->
    plot; plot

custom_ggsave(p = plot, DIR = FIGS_DIR, name = "hist_prDelta_temp", WIDTH = 8, HEIGHT = 4)


rbind(out1, out2) %>%
    filter(year %in% 1850:2015) %>%
    distinct() %>%
    filter(variable %in% GLOBAL_TAS()) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line(linewidth = 1) +
    theme(legend.title = element_blank()) +
    labs(x = "Year", y = "Deg C") ->
    plot; plot




# Description: Generate time series figures for the H2 manuscript that rely
# on stand alone hector only. Figures and results will be generated
# and written out to disk.

# 0. Set Up --------------------------------------------------------------------
source("scripts/0.env_fxns.R")

# Dates to save
RSLT_DATES <- 1750:2022

# Variables to save
RSLT_VARS <- c("RF_CO2", "RF_tot", "global_tas", "TAU_OH",  "RF_O3_trop", RF_CH4(),
               "ffi_emissions", "RF_H2O_strat",  "TAU_OH", "rh_ch4", CONCENTRATIONS_CH4(),
               CONCENTRATIONS_CO2(), CONCENTRATIONS_O3())


# 1. Load & Check Inputs  ------------------------------------------------------

# Check to make sure that the historical h2 inputs have been added to the
# gcam input tables. If not then will need to modify the gcam_emissions.csv file.

# Load the historical inventory in hector units.
hist_h2_inputs <- read.csv("data/hist_h2_inputs.csv")

# Load the GCAM emission input table.
read.csv("inputs/gcam_emissions.csv", comment.char = ";") %>%
    select(c("Date", EMISSIONS_H2())) %>%
    rename(year = Date, gcam_h2 = EMISSIONS_H2())  ->
    gcam_emission_inputs

# okay this honestly surprises me! I would have expected this value to be 0!!
hist_h2_inputs %>%
    select(year, h2_inventory = value) %>%
    left_join(gcam_emission_inputs) %>%
    mutate(error = abs(h2_inventory-gcam_h2)) %>%
    pull(error) %>%
    max ->
    max_err

# Confirm that the error is the correct size, if an error is thrown here
# users will need to update the inputs/gcam_emissions.csv to match the
# H2 Emissions saved in data/hist_h2_inputs.csv.
stopifnot(max_err < 1e-6)

# 2. H2 Emissions Inventory ---------------------------------------------------------

# Plot the H2 Emissions Inventory
ggplot() +
    geom_line(data = hist_h2_inputs, aes(year, value)) +
    labs(y = "Tg",
         x = NULL) +
    theme(legend.title = element_blank()) ->
    plot; plot

custom_ggsave(p = plot, DIR = DIRS$RSLTS, name = "Fig_1", WIDTH = 8, HEIGHT = 4)

# 3. Hector Runs ---------------------------------------------------------------

# The baseline or control run.
ini <- "inputs/hector-gcam_noh2.ini"
hc  <- newcore(ini)
run(hc, runtodate = max(RSLT_DATES))
noh2 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
noh2$scenario <- "default"
shutdown(hc)

# With the H2 indirect effects activated.
ini <- "inputs/hector-gcam.ini"
hc  <- newcore(ini)
run(hc, runtodate = max(RSLT_DATES))
withh2 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
withh2$scenario <-  "h2 effects"


# 4. RF Results ------------------------------------------------------------------
# Plot the change in historical radiative forcing with and without H2 Chemistry,
# broken up by the RF type.
VAR <- RF_TOTAL()
UNITS <- unique(out1[out1$variable == VAR,]$units)

rbind(noh2, withh2) %>%
    filter(variable == VAR) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() +
    labs(title = VAR,
         y = UNITS,
         x = NULL) ->
    total_RF

rbind(noh2, withh2) %>%
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

custom_ggsave(p = plot, DIR = DIRS$RSLTS, name = "Fig_4", WIDTH = 8, HEIGHT = 4)

rbind(noh2, withh2) %>%
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




# 5. Concentrations Change -----------------------------------------------------
#  "ppbv CH4" "ppmv CO2" "DU O3"

rbind(noh2, withh2) %>%
    filter(year %in% 1850:2022) %>%
    distinct() %>%
    filter(variable %in% c(CONCENTRATIONS_CH4(), CONCENTRATIONS_CO2(), CONCENTRATIONS_O3())) %>%
    spread(scenario, value) %>%
    mutate(diff = 100 * (`h2 effects` - default)/default) ->
    diff_df

diff_df$year %>% range()

diff_df %>%
    summarise(min  = min(diff),
              max = max(diff),
              mean = mean(diff), .by = "variable") %>%
    mutate(units = "%") %>%
    knitr::kable(digits = 4) ->
    precent_change_conc

write_results(name = "Historical Conc % Change", val = precent_change_conc)


diff_df %>%
    ggplot(aes(year, diff, color = variable)) +
    geom_line(linewidth = 1) +
    theme(legend.title = element_blank()) +
    labs(x = "Year", y = "% Difference") ->
    plot; plot

custom_ggsave(p = plot, DIR = DIRS$RSLTS, name = "Fig_3", WIDTH = 8, HEIGHT = 4)


# 6. Temperature Change -----------------------------------------------------
rbind(noh2, withh2) %>%
    filter(year %in% 1850:2015) %>%
    distinct() %>%
    filter(variable %in% GLOBAL_TAS()) %>%
    spread(scenario, value) %>%
    mutate(diff = (`h2 effects` - default)) ->
    diff_df


# Mean change in historical temperature
mean_change <- mean(diff_df$diff)

write_results(name = "Mean Hist Change in Temp", val = mean_change)

diff_df %>%
    ggplot(aes(year, diff, color = variable)) +
    geom_line(linewidth = 1) +
    theme(legend.title = element_blank()) +
    labs(x = "Year", y = "Temperature Difference (Deg C)") ->
    plot; plot

custom_ggsave(p = plot, DIR = DIRS$RSLTS, name = "Fig_5", WIDTH = 8, HEIGHT = 4)


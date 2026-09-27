# Description: Set up the H2 inputs for hector.

# 0. Set Up --------------------------------------------------------------------
source("scripts/0.env_fxns.R")

# 1. Set Up --------------------------------------------------------------------

# Convert from kt to Tg of H2
Kt_Tg <- 0.001

# Load P. ORourke historical H2 emissions inventory.
"data/D.H2_emissions_global_total-GCAM.csv" %>%
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
    rbind(hist_h2_inputs) %>%
    arrange(year) ->
    hist_h2_inputs

# These inputs will be used in the hector run and data visualization.
write.csv(hist_h2_inputs, file = "data/hist_h2_inputs.csv", row.names = FALSE)







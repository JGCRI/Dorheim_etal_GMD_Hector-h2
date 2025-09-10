

library(tidyr)


# Convert from kt to Tg of H2
Kt_Tg <- 0.001


file <- "data/D.H2_emissions_global_total.csv"
read.csv(file) %>%
    pivot_longer(values_to = "value", cols = starts_with("X")) %>%
    mutate(year = as.integer(gsub(x = name, replacement = "", pattern = "X"))) %>%
    select(value, year) %>%
    mutate(value = Kt_Tg * value) %>%
    mutate(units = getunits(EMISSIONS_H2()),
           variable = EMISSIONS_H2()) ->
    hist_h2_inputs


RSLT_DATES <- 1750:2022
RSLT_VARS <- c("RF_CO2", "RF_tot", "global_tas", "TAU_OH",  "RF_O3_trop", RF_CH4(),
               "ffi_emissions", "RF_H2O_strat",  "TAU_OH", "rh_ch4", CONCENTRATIONS_CH4(), CONCENTRATIONS_CO2())

ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc  <- newcore(ini)
run(hc, runtodate = max(RSLT_DATES))
out1 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
out1$scenario <- "default"

# now run with h2 emissions
setvar(hc, dates = hist_h2_inputs$year, var = hist_h2_inputs$variable,
       values = hist_h2_inputs$value, unit = hist_h2_inputs$units)
reset(hc)
run(hc)

out2 <- fetchvars(hc, RSLT_DATES, RSLT_VARS)
out2$scenario <- "h2 effects"


VAR <- RF_TOTAL()
UNITS <- unique(out1[out1$variable == VAR,]$units)


rbind(out1, out2) %>%
    filter(variable == VAR) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() +
    labs(title = VAR,
         y = UNITS,
         x = NULL)


rbind(out1, out2) %>%
    distinct() %>%
    filter(grepl(pattern = "RF", x = variable)) %>%
    spread(scenario, value) %>%
    mutate(diff = `h2 effects` - default) ->
    diff_df

diff_df   %>%
    filter(variable != RF_TOTAL()) ->
    componets
diff_df   %>%
    filter(variable == RF_TOTAL()) ->
    total


    ggplot() +
    geom_area(data = componets, aes(year, diff, fill = variable)) +
    geom_line(data = total, aes(year, diff, color = "Total RF"), size = 1) +
    labs(y = UNITS,
         title = "RF",
         subtitle = "h2 effects - default",
         x = NULL) +
        scale_color_manual(values = c("Total RF" = "black"))



    rbind(out1, out2) %>%
        distinct() %>%
        spread(scenario, value) %>%
        mutate(AE = abs(`h2 effects` - default)) %>%
        summarise(MAE = mean(AE), .by = "variable")

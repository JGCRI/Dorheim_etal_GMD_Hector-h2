# Test to see if the results coming out of gcam are different or not,
# although it is unclear why some of the tables are incorrect but I hink
# that has to do with the quieries being run and then also what is going on
# with the model interface...


library(rgcam)
library(dplyr)
library(ggplot2)

conn <- localDBConn("../gcam-core/output/", "database_basexdb")
listScenariosInDB(conn)

# Extract the original GCAM results aka before the H2 emissions were passed to GCAM
project.data <- addScenario(conn, 'project.dat', 'Reference-noH2 2024-6-11T21:38:28+19:00')
project.data <- loadProject('project.dat')

project.data$`Reference-noH2`$`Climate forcing` %>%
    mutate(scenario = "default") ->
    climate_forcing1



# Extract the original GCAM results after phase 1 of the hector dev, we would
# expect the H2 emissions to have some effect on the cliamte results

# Extract the original GCAM results aka before the H2 emissions were passed to GCAM
project.data2 <- addScenario(conn, 'project.dat2')
project.data2 <- loadProject('project.dat2')

project.data2$`Reference-noH2`$`Climate forcing` %>%
    mutate(scenario = "phase1") ->
    climate_forcing2



rbind(climate_forcing2, climate_forcing1) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line()


wide <-climate_forcing2
wide$diff <- climate_forcing1$value - climate_forcing2$value

ggplot(data = wide) +
    geom_line(aes(year, diff))


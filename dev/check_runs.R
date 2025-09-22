# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")



ini <- "inputs/hector_picontrol.ini"
hc <- newcore(ini)
run(hc, runtodate = 2100)



ini <- "inputs/picontrol_ch4-emiss.ini"
hc <- newcore(ini)
run(hc, runtodate = 2100)

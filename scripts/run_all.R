# Run all the analysis and data visualization scripts for the manuscript.

files <- c("GWP.R", "historical.R")

for(f in files){

    source(here::here("scripts", f))
}


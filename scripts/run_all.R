# Run all the analysis and data visualization scripts for the manuscript.

files <- c("1.inputs.R","2.GWP.R", "2.historical.R" )

for(f in files){

    source(here::here("scripts", f))
}


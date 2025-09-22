# Define the global constants and environment for this project.
# TODO
# Should probably switch to renv.
# Will need to make sure that the correct version of Hector is being called.

HECTOR_DIR <- "../hector"

devtools::load_all(HECTOR_DIR)

library(ggplot2)
library(dplyr)
library(RColorBrewer)


theme_set(theme_bw())

COLORS <- brewer.pal(n = 8, name = 'Dark2')


# Quick save the session info
session_info <- sessionInfo()
output_file <- "session_info.txt"
sink(output_file)
print(session_info)
sink()


# Custom helper function that saves a plot and the data used in the figure
# Args
#   p: ggplot object to save
#   DIR: location where to write the figure and the csv out to
#   name: base name for the figure and the data
#   type: pdf (or png) the type of figure to save
#   WIDTH: default set to 10, controls the width of the figure
#   HEIGHT: default set to 5, controls the height of the figure
custom_ggsave <- function(p, DIR, name, type = "png", WIDTH = 10, HEIGHT = 5){

    # Make sure the directory exists before setting
    # up the file name!
    stopifnot(dir.exists(DIR))
    stopifnot(any(type %in% c("pdf", "png")))

    # Save a copy of the files and the underlying data for the figure!
    ggsave(plot = p, filename = file.path(DIR, paste0(name, ".", type )), width = WIDTH, height = HEIGHT)

    if(is.data.frame(p$data)){
        write.csv(p$data, file = file.path(DIR, paste0(name, "_data.csv")), row.names = FALSE)
    } else if(class(p$data) == "waiver") {

        # We have a layered data set! So will need to save the data individually
      n_layers <- length(p$layers)

      for(i in 1:n_layers){

          out <- p$layers[[i]]$data
          write.csv(out, file = file.path(DIR, paste0(name, "_data_L",i, ".csv")), row.names = FALSE)

      }


    } else {
        warning("no data saved")}

}







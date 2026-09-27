# Define the global constants and environment for this project.
# TODO: could switch to renv.
# 0. Set Up --------------------------------------------------------------------

# Load all the standard R packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(here)

# Make sure that the correct version of hector is installed
# TODO update the remotes install call with the specific release tag.
remotes::install_github("jgcri/hector@dev-h2")
library(hector)
stopifnot(packageVersion("hector") == '3.6.0')


# Define some directories
DIRS <- list()
DIRS[["RSLTS"]] <- here::here("rslts")
dir.create(DIRS$RSLTS, showWarnings = FALSE)

# Set up some aesthetics
theme_set(theme_bw())
COLORS <- brewer.pal(n = 8, name = 'Dark2')

# 1. Save some session info ----------------------------------------------------

# Quick save the session info
session_info <- sessionInfo()
output_file <- "session_info.txt"
sink(output_file)
print(session_info)
sink()

# 2. Helpful functions ---------------------------------------------------------

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
        write.csv(p$data, file = file.path(DIR, "fig-data", paste0(name, "_data.csv")), row.names = FALSE)
    } else if(class(p$data) == "waiver") {

        # We have a layered data set! So will need to save the data individually
      n_layers <- length(p$layers)

      for(i in 1:n_layers){

          out <- p$layers[[i]]$data
          write.csv(out, file = file.path(DIR, "fig-data", paste0(name, "_data_L",i, ".csv")), row.names = FALSE)

      }


    } else {
        warning("no data saved")}

}


write_results <- function(name, val, info = NULL) {


    # function that writes results mentioned in the text of the manuscript
    # out to a single file. Each file will be time stamped.
    #
    # Args
    #   name: name of value to include the results file
    #   val: value of metric or a small table to to write out
    #   info: string of some additional information if it would be helpful
    #
    # Returns: nothing but a txt file should be save to the root directory
    #
    # Note: The value of the metric is rounded to 3 sig figs when outputted


    timestamp <- format(Sys.time(), "%a_%b_%d_%H00_%Y")
    fname <- file.path("rslts", paste0(paste("rslts", timestamp, sep = "_"), ".txt"))

    if(length(val) == 1){

        line <- paste(name, val, info)
        write(line, file = fname, append = TRUE)

    }

    if(is.data.frame(val)){

        write("-----------------------", file = fname, append = TRUE)
        write(name, file = fname, append = TRUE)

        write.table(x = val, file = fname, append = TRUE,quote = FALSE, row.names = FALSE, sep = ", ")
        write("-----------------------", file = fname, append = TRUE)

    }


}

#' Function to do telemetry analysis
#'
#' @param x Dummy param.
#'
#' @return
#' @export
#'
#' @examples
telemetry <- function(x) {
  x <- x + 1
}

library(remotes)
remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
library(actel)
# parallel sections bug warning https://hugomflavio.github.io/actel-website/issue_79.html


# organise data ####
# https://hugomflavio.github.io/actel-website/manual-data.html
# you should have a file named biometrics.csv, one named spatial.csv, and one named deployments.csv
# you also need a detections directory, containing all your detection files in .csv format.
# To create a blank workspace ready to be used, run blankWorkspace(), and a template will be
# generated automatically in an "actel_workspace" directory. If you would like to, you can change
# the name of the target directory by using the argument dir. Remember to delete the example rows
# before including your data!

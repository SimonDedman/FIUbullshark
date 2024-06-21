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


# 2024-01-29
# Opened vdb file with Vemco Vue software on Win10 vbox, exported Detections & Events tabs to csv.
# Created biometrics, deployments, and spatial csvs from Bull Shark Data.xlsx
# https://hugomflavio.github.io/actel-website/manual-data.html
loadloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data/"
loadloc <- "C:/Users/davon/OneDrive - Florida International University/FIU Bull Sharks/DATA"

biometrics <- readr::read_csv(file.path(loadloc_davon, "biometrics.csv")) |>
  dplyr::mutate(Sex = as.character("Female"))
spatial <- readr::read_csv(file.path(loadloc, "spatial.csv"))
deployments <- readr::read_csv(file.path(loadloc, "deployments.csv"))
detections <- readr::read_csv(file.path(loadloc, "FWCdepredationStudy-Detections.csv"))
mypreload <- actel::preload(biometrics = biometrics,
                            spatial = spatial,
                            deployments = deployments,
                            detections = detections,
                            tz = "UTC")
# Error: Some animals have no 'Signal' information. Please double-check the biometrics.

# Get summary of data
actel::explore(tz = "America/New_York",
               datapack = mypreload)

explore(
  tz = NULL,
  datapack = NULL,
  max.interval = 60,
  minimum.detections,
  min.total.detections = 2,
  min.per.event = 1,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = FALSE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays")
)

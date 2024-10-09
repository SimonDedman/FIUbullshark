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

library(tidylog)
library(remotes)

loadloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data"
loadloc <- "C:/Users/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data"
loadloc <- "C:/Users/davon/OneDrive - Florida International University/FIU Bull Sharks/DATA"

remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
library(actel)
# parallel sections bug warning https://hugomflavio.github.io/actel-website/issue_79.html
# create blank workspace for ACTEL which allows dumping of csv files
actel::blankWorkspace(dir = file.path(loadloc, "actel"))


# Organise data ####
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

## Biometrics file ####
# Release.date: date & time animal released, yyyy-mm-dd hh:mm:ss.
## Timestamps must be local tz study area, supply in tz argument.
# Signal: code emitted by tags. Ask tag manufacturer about differences between code spaces and signals.
# All animals need a Signal value else function crashes; flag & remove NAs

print(paste0(length(which(is.na(biometrics$Signal))), " animals missing Signal values, which will be removed"))
biometrics <- readr::read_csv(file.path(loadloc, "actel", "biometrics.csv")) |>
  dplyr::mutate(Sex = as.character("Female"),
                Shark = stringr::str_to_title(Shark),
                Location = stringr::str_to_title(Location),
                # TODO columns to factors asked ####
                # Make Shark, Sex, Release.site into factors? ASKED
                Signal = as.integer(Signal),
                ExternalTag = as.integer(ExternalTag),
                Release.date = as.POSIXct(Release.date, format = "%d/%m/%Y %H:%M", tz = "UTC")) |>
  dplyr::rename(Release.site = Location,
                # Existing columns length, weight, mass: distribution graphics drawn per animal group in report.
                # length = Length.mm
                Group = Shark
  ) |>
  # dplyr::filter(Group == "Bull") |> # remove non bulls for now
  # TODO column name format asked ####
# check if this can be "Length" rather than "length"? ASKED
tidyr::drop_na(Signal)




## Deployments file ####
deployments <- readr::read_csv(file.path(loadloc, "actel",  "deployments.csv")) |>
  dplyr::mutate(Station.name = stringr::str_to_title(Station.name),
                Start = as.POSIXct(Start, format = "%d/%m/%Y %H:%M"),
                Stop = as.POSIXct(Stop, format = "%d/%m/%Y %H:%M"),
                Station.name = dplyr::case_match(
                  Station.name,
                  "Mg111" ~ "MG111",
                  .default = Station.name)) |>
  tidyr::drop_na(Start, Stop) # remove undownloaded receivers as they crash preload





## Detections file ####
detections <- readr::read_csv(file.path(loadloc, "FWCdepredationStudy-Detections.csv")) |>
  dplyr::select(!dplyr::where(~all(is.na(.)))) |>
  dplyr::mutate(Signal = as.integer(stringr::str_sub(string = CodeSpace,
                                                     start = 10,
                                                     end = -1)),
                CodeSpace = stringr::str_sub(string = CodeSpace,
                                             start = 1,
                                             end = 8),
                Receiver = as.integer(Receiver),
                Station.name = dplyr::case_match(
                  Station.name,
                  "Mg111" ~ "MG111",
                  .default = Station.name),
                Timestamp = as.POSIXct(Timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC"))
# doesn't have seconds so won't match on seconds.

### Covert VRL files ####
# https://stackoverflow.com/questions/67745382/vemco-acoustic-telemetry-data-vrl-files-in-r
# remotes::install_github("ocean-tracking-network/glatos")
library(glatos)
csvfiles <- glatos::vrl2csv(vrl = file.path(loadloc, "VRL/"),
                            outDir = file.path(loadloc, "VRL/"),
                            vueExePath = "C:/Program Files (x86)/VEMCO VUE")
csvfiles <- list.files(path = file.path(loadloc, "VRL"),
                       pattern = ".csv",
                       full.names = TRUE)
# remove blank files from list, don't import
csvfiles <- csvfiles[c(7:13)]

# 111962 AllDetections rows with VRL2CSV convert only, no FACT csvs.
AllDetections <- do.call(rbind, lapply(csvfiles, read.csv)) |>
  # remove NA columns: transmitter name, transmitter serial, station name, lat, lon, transmitter type, sensor precision.
  dplyr::select(!dplyr::where(~all(is.na(.)))) |>
  dplyr::mutate(Timestamp = as.POSIXct(Date.and.Time..UTC.,
                                       format = "%Y-%m-%d %H:%M:%S", # all bar VR2W_139045_20240923_1.csv work
                                       tz = "UTC"),  # "America/New_York"
                # Is the last block of numbers in detections$CodeSpace the same as (no NA) biometrics$Serial?
                # all(biometrics$Serial[which(!is.na(biometrics$Serial))] %in% as.numeric(unique(stringr::str_sub(string = detections$CodeSpace,
                # start = 10, end = -1)))) # TRUE
                Signal = as.numeric(stringr::str_sub(string = Transmitter,
                                                     start = 10,
                                                     end = -1)),
                CodeSpace = stringr::str_sub(string = Transmitter,
                                             start = 1,
                                             end = 8),
                Receiver = as.numeric(stringr::str_sub(string = Receiver,
                                                       start = 6,
                                                       end = -1))) |>
  tidyr::drop_na(Signal) |>
  dplyr::select(Timestamp, Receiver, CodeSpace, Signal, Sensor.Value, Sensor.Unit)

# check overlap of old detections with new AllDetections
oldNotInNew <- detections[which(!detections$Timestamp %in% AllDetections$Timestamp),] #|>
# no longer matches any because AllDetections has seconds and detections doesn't.
# dplyr::mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
# 4 present which are oddly NA timestamped but clearly those entries. Manually enter.
# AllDetections$Timestamp[29929:29932] <- oldNotInNew$Timestamp
# AllDetections$Timestamp[28391:28393] <- oldNotInNew$Timestamp
# rm(oldNotInNew)

rm(AllDetections)

# loads of NA timestamps at the bottom of AllDetections: find which csv responsible, investigate.
# VR2W_139045_20240923_1.csv dates not parsing but all look fine, same as other sheets. Are formatted the same.
# Wrote a massive stackoverflow post about it, wasted 3 hours diagnosing, turns out it's excel fucking things up.
# Just re-run glatos to correct them.
# which(is.na(AllDetections$Timestamp))
# tmp <- AllDetections[which(is.na(AllDetections$Timestamp)),]

### read FACT detections ####
factfiles <- list.files(path = file.path(loadloc, "FACT"),
                        pattern = ".csv",
                        full.names = TRUE)

library(magrittr)
FactDetections <- do.call(rbind, lapply(factfiles, read.csv)) |>
  dplyr::select(!dplyr::where(~all(is.na(.)))) |>
  dplyr::rename(CodeSpace = codespace,
                Receiver = receiver,
                Station.name = station,
                Latitude = latitude,
                Longitude = longitude) |>
  dplyr::filter(Receiver != "release") |> # removes 15 rows, allows integer
  dplyr::mutate(
    Timestamp = as.POSIXct(x = paste(paste(yearcollected,
                                           monthcollected,
                                           daycollected,
                                           sep = "-"),
                                     paste(floor(timeofday),
                                           floor((timeofday - floor(timeofday)) * 60), # decimal minutes,
                                           (((timeofday - floor(timeofday)) * 60) - floor((timeofday - floor(timeofday)) * 60)) * 60, # decimal seconds
                                           sep = ":"),
                                     sep = " "),
                           tz = "UTC"),
    Start = min(Timestamp, na.rm = TRUE), # for deployments, default populate with earliest possible date
    Stop = as.POSIXct(datelastmodified, format = "%m/%d/%Y", tz = "UTC"), # for deployments, when receivers were downloaded
    Receiver = as.integer(Receiver),
    Group = stringr::str_to_title(dplyr::case_match(
      commonname,
      "bull shark" ~ "Bull",
      .default = commonname)),
    Signal = as.numeric(stringr::str_sub(string = tagname,
                                         start = 10,
                                         end = -1)),
    Sensor.Unit = as.character(NA),
    Section = "Ocean", # for spatial
    Array = "A1", # for spatial
    Type = "Hydrophone" # for spatial
  ) %T>%
  {. |> dplyr::arrange(Receiver) |>
      dplyr::group_by(Receiver, Station.name, Stop) |>
      dplyr::summarise_all(dplyr::first) |>
      dplyr::select(Receiver, Station.name, Start, Stop) |>
      dplyr::arrange(Station.name, Stop) ->> factDeployments} %T>% # export changes to deployments
  {. |> dplyr::select(Station.name, Latitude, Longitude, Section, Array, Type) |>
      dplyr::arrange(Station.name) |>
      dplyr::group_by(Station.name, Latitude, Longitude) |>
      dplyr::summarise_all(dplyr::first) ->> factSpatial} |>
  dplyr::select(Timestamp, Receiver, CodeSpace, Signal, Sensor.Unit, Group)

# overwrite start dates for stations with 2+ entries
# factDeployments |>
#   dplyr::group_by(Station.name) |>
#   dplyr::summarise(n = dplyr::n(),
#                    Start = dplyr::first(Stop)) |>
#   dplyr::filter(n > 1) |>
#   dplyr::select(-n)
factDeployments$Start[which(duplicated(x = factDeployments$Station.name))] <- factDeployments$Stop[which(duplicated(x = factDeployments$Station.name)) - 1]

# tmp <- detections[which(duplicated(detections)),]


## Deployments file part 2 ####
deployments <- deployments |>
  dplyr::select(-Notes) |>
  dplyr::bind_rows(factDeployments)
# TODO: merge FACT/FIU station dupes####
### Fix deployment dupes date overlap issues ####
deployments[deployments$Receiver == 138239 & deployments$Station.name == "3", "Start"] <- deployments[deployments$Receiver == 138239 & deployments$Station.name == "Princess Anne", "Stop"]
deployments[deployments$Receiver == 138240 & deployments$Station.name == "1", "Start"] <- deployments[deployments$Receiver == 138240 & deployments$Station.name == "St Jaques", "Stop"]



# Do we have a download date?
# tmp <- FactDetections |> group_by(receiver_group, station) |> summarise(n = dplyr::n(), contact_pi = dplyr::first(contact_pi))
# write.csv(x = tmp,file = file.path(loadloc, "FactStations.csv"), row.names = FALSE)




## Spatial file ####
spatial <- readr::read_csv(file.path(loadloc, "actel", "spatial.csv")) |>
  dplyr::mutate(Station.name = stringr::str_to_title(Station.name),
                Station.name = dplyr::case_match(
                  Station.name,
                  "Mg111" ~ "MG111",
                  .default = Station.name)) |>
  dplyr::bind_rows(factSpatial) |>
  dplyr::group_by(Station.name) |> # dedupe stations
  dplyr::summarise_all(dplyr::first) #|>
# Error: The following station is listed in the spatial file but no receivers were ever deployed there: 'Deep Ledge'
# dplyr::filter(Station.name != "Deep Ledge")
# St Jaques, Governors Wrecks, 1, all same latlon


library(gbm.auto)
# create shapefile and respective auxiliary files in the folder where you have your 'spatial.csv'.
gbm.auto::gbm.basemap(bounds = c(min(spatial$Longitude),
                                 max(spatial$Longitude),
                                 min(spatial$Latitude),
                                 max(spatial$Latitude)),
                      savedir = file.path(loadloc, "actel"),
                      extrabounds = TRUE)
# convert to raster
base.raster <- actel::shapeToRaster(shape, # path to your shapefile, including the ".shp" extension
                                    size, # of the raster pixels in metres
                                    spatial = spatial, # " character string specifying the path to a spatial.csv file or a spatial data frame
                                    coord.x = "Longitude", # names of the columns containing the x and y coordinates
                                    coord.y = "Latitude",
                                    buffer = NULL) # request an expansion of the shapefile limits
# could use gbm.auto code to do this easily if Hugo wants, depending on how it goes for me.


# remove stations with no deployments (possibly due to not being downloaded thus removed)
spatial <- spatial |> dplyr::filter(Station.name %in% deployments$Station.name)


# Create distance matrix ####
# https://hugomflavio.github.io/actel-website/manual-distances.html
# create a shapefile that extends over all your receivers and release sites
distances <- actel::distancesMatrix()







# Preload ####
mypreload <- actel::preload(biometrics = biometrics,
                            spatial = spatial,
                            deployments = deployments,
                            detections = detections,
                            # distances = distances,
                            tz = "UTC") # America/New_York
# M: Preloaded Release dates are already in POSIX format. Skipping timestamp format checks.
# Warning: Potential mismatch between release dates time zone (UTC) and 'tz' argument (America/New_York)! This could cause unwanted timelapses!
#   M: No 'Group' column found in the biometrics. Assigning all animals to group 'All'.
# M: Number of target tags: 7.
# M: Preloaded deployment times are already in POSIX format. Skipping timestamp format checks.
# Warning: Potential mismatch between deployments time zone () and 'tz' argument (America/New_York)! This could cause unwanted timelapses!
#   Warning: The 'Receiver' column in the detections is not of type integer. Attempting to convert.
# Warning: Attempting to convert the 'Receiver' to integer failed. Attempting to extract only the serial numbers.
# Warning: Could not find a 'Sensor.Value' column in the detections. Filling one with NA.
# M: Detection timestamps are already POSIX and time zone already matches tz argument. Skipping time zone matching.
# Warning: Release sites were not specified in the spatial.csv file. Attempting to assume all released animals start at the top level array.
# M: Matching detections with deployment periods.
# |===================================================         |  84%
# Error: 423 detections for receiver 138240 do not fall within deployment periods.
#
# Timestamp Receiver CodeSpace Signal Sensor.Unit Sensor.Value
# <POSc>   <char>    <char>  <int>      <char>        <num>
#   1: 2023-11-19 19:06:58   138240      5518     43         ADC           NA
# 2: 2023-11-30 09:24:24   138240      5518     43         ADC           NA
# 3: 2024-01-04 22:37:19   138240      5518     43         ADC           NA
# 4: 2024-02-10 07:58:48   138240      2506     68         ADC           NA
# 5: 2024-02-10 08:02:20   138240      2506     68         ADC           NA
# ---
#   419: 2024-03-14 11:38:18   138240      4229     82         ADC           NA
# 420: 2024-03-14 11:47:18   138240      4228    106         ADC           NA
# 421: 2024-03-14 12:00:18   138240      4229     45         ADC           NA
# 422: 2024-03-14 12:01:07   138240      4228     99         ADC           NA
# 423: 2024-03-14 12:02:00   138240      4229     35         ADC           NA
#
# Possible options:
# a) Stop and double-check the data (recommended)
# b) Discard orphan detections in this instance.
# c) Discard orphan detections for all instances.
# d) Save orphan detections to a file and re-open dialogue.
# b
# |=======================================================     |  92%
# M: Number of ALS: 3 (of which 1 had no detections)
# Warning: No detections were found for receiver(s) 139041.
# M: Data time range: 2022-06-09 11:47:11 to 2023-05-19 20:57:08 (America/New_York).
# Error: No detections were found in the input data which matched the target signals.

# sort(unique(detections$Signal))[which(sort(unique(detections$Signal)) %in% sort(unique(biometrics$Signal)))]
# 34  36 204

# St Jacques deployment stops 2023-07-14, 423 rows after that, from where?

# 2024-10-01
# M: Preloaded Release dates are already in POSIX format. Skipping timestamp format checks.
# M: No Code.space column was found in the biometrics. Assigning code spaces based on detections.
# M: Number of target tags: 15.
# M: Preloaded deployment times are already in POSIX format. Skipping timestamp format checks.
# Warning: 121 duplicated detections were found. Could an input file be duplicated?
# Possible options:
# a) Stop and double-check the data
# b) Remove duplicated detections ***
# c) Continue without changes
# d) Save duplicated detections to a file and re-open dialogue.

# Error: The following release sites were listed in the biometrics.csv file
# but are not part of the release sites listed in the spatial.csv file:
# Deep_Ledge, Jupiter
# Please include the missing release sites in the spatial.csv file.

# M: Number of ALS: 47 (of which 44 had no detections)
# Warning: No detections were found for receiver(s) 104532, 104790, 107564, 107888, 110349, 123804, 124489, 128002, 128979, 128980, 130011, 130014, 130020, 130021, 130023, 130749, 130781, 130791, 132316, 132753, 134302, 134306, 134314, 134318, 134618, 136423, 136424, 136536, 137295, 139045, 484727, 484728, 484731, 484799, 484823, 485925, 485926, 485928, 485931, 487182, 487183, 487185, 487189, 489693.
# M: Data time range: 2022-05-10 19:48:00 to 2024-05-16 04:31:00 (UTC).
# M: Extracting relevant detections...
# Stray tags were detected in your study area. Would you like to save a summary to stray_tags.csv?(y/n)
# [1] "C:/Users/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Code/FIUbullshark/stray_tags.csv"




# Summary & Explore ####
# Get summary of data
install.packages("gWidgets2tcltk")
library(gWidgets2tcltk)
getwd()
dir.create(file.path("..", "..", "data", "actel", "explore"))
setwd(file.path("..", "..", "data", "actel", "explore"))
exploreResults <- actel::explore(datapack = mypreload,
                                 tz = NULL, # tz = "America/New_York", not needed if datapack provided
                                 max.interval = 60, # number of minutes that must pass between detections for a new event to be created
                                 min.total.detections = 2,
                                 min.per.event = 1,
                                 start.time = NULL, # Detection data prior to the timestamp set in start.time (in YYYY-MM-DD HH:MM:SS format) is not considered during the analysis
                                 stop.time = NULL, # ditto, posterior
                                 speed.method = c("last to first", "last to last", "first to first"),
                                 speed.warning = NULL, # warns if speed in m/s > this value. <= speed.error
                                 speed.error = NULL, # ditto but suggests user input
                                 jump.warning = 2, # warns if jumping arrays w/o detection
                                 jump.error = 3, # ditto, errors
                                 inactive.warning = NULL, # n days inactive
                                 inactive.error = NULL, # ditto, error
                                 exclude.tags = NULL, # vector of tags, different codespace but same signal detected
                                 override = NULL, # vector of signals user defined as in/valid
                                 report = TRUE, # generate HTML report?
                                 auto.open = TRUE, # open report?
                                 discard.orphans = FALSE, # detections outside receiver deployment periods or before animal release dates
                                 discard.first = NULL, # hours after release to begin counting detections as valid
                                 save.detections = FALSE, # save processed detections for future runs?
                                 ## HUGO NOTE: manpage unclear why user would want this ####
                                 GUI = c("needed", "always", "never"),
                                 save.tables.locally = FALSE, # only if too big for R console/whenever possibility to invalidate events/never
                                 print.releases = TRUE, # print release sites on study area diagrams
                                 detections.y.axis = c("auto", "stations", "arrays")) # for individual detection plots

# M: Creating movement records for the valid tags.
# M: Checking movement events quality.
# Warning: 'speed.warning'/'speed.error' were not set, skipping speed checks.
# Warning: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.
# Warning: Tag A69-9001-52503 (1/6) has less than 2 detections in total. Discarding this tag.


# Migration ####
migrationsResults <- actel::migration(
  tz = NULL,
  section.order = NULL, # vector containing the order by which sections should be aligned in the results
  datapack = mypreload,
  success.arrays = NULL, # arrays that mark the end of the study area. If a tag crosses one of these arrays, the respective animal is considered to have successfully migrated through the study area
  max.interval = 60,
  min.total.detections = 2,
  min.per.event = 1,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last", "first to first"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = TRUE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  if.last.skip.section = TRUE, # Should a tag detected at the last array of a given section be considered to have disappeared in the next section?
  replicates = NULL,
  disregard.parallels = TRUE, # Should the presence of parallel arrays invalidate potential efficiency peers?
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays")
)
# M: Running analysis on preloaded data (compiled on 2024-10-01 14:21:21.630737).
# M: 'disregard.parallels' is set to FALSE; the presence of parallel arrays can potentially invalidate efficiency peers.
# Warning: 'success.arrays' was not defined. Assuming success if the tags are last detected at array A1.
# Error in apply(x, 1, function(i) i[1] != i[2]): dim(X) must have a positive length
# M: The analysis errored. You can recover latest the job log (including your comments and decisions) by running recoverLog().



# Residency ####
residencyResults <- actel::residency(
  tz = NULL,
  section.order = NULL,
  datapack = mypreload,
  max.interval = 60,
  min.total.detections = 2,
  min.per.event = 1,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last", "first to first"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = TRUE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  section.warning = 1, # warns for section movement events with <= this value
  section.error = 1,
  timestep = c("days", "hours"),
  replicates = NULL,
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays")
)

M: Producing the report.
Error in x[, sections[link]] <- 0 : incorrect number of subscripts on matrix

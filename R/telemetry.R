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

loadloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data" # si linux
loadloc <- "C:/Users/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data" # si windows
loadloc <- "C:/Users/davon/OneDrive - Florida International University/FIU Bull Sharks/DATA" # davon

remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
library(actel)
# parallel sections bug warning https://hugomflavio.github.io/actel-website/issue_79.html
# create blank workspace for ACTEL which allows dumping of csv files
# actel::blankWorkspace(dir = file.path(loadloc, "actel"))


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

## 1. Biometrics file ####
# Release.date: date & time animal released, yyyy-mm-dd hh:mm:ss.
## Timestamps must be local tz study area, supply in tz argument.
# Signal: code emitted by tags. Ask tag manufacturer about differences between code spaces and signals.
# All animals need a Signal value else function crashes; flag & remove NAs

# print(paste0(length(which(is.na(biometrics$Signal))), " animals missing Signal values, which will be removed"))
biometrics <- readr::read_csv(file.path(loadloc, "actel", "biometrics.csv")) |>
  dplyr::mutate(
    time_tagged = as.character(time_tagged),
    time_tagged = dplyr::case_match(
      time_tagged,
      NA ~ as.character("00:00:00"),
      .default = time_tagged),
    Release.date = as.POSIXct(paste(date_encountered, time_tagged),
                              format = "%Y-%m-%d %H:%M",
                              tz = "America/New_York"),
    Release.date = lubridate::with_tz(Release.date, tzone = "UTC"), # change to UTC
    tag_location = stringr::str_to_title(tag_location),
    # species = stringr::str_to_title(species),
    # create 'max 6 character' species/Group alternative
    Group = dplyr::case_match(
      species,
      "Caribbean reef" ~ "Crb_Rf",
      "Sandbar" ~ "Sndbar",
      .default = species), # "Bull"
    sex = dplyr::case_match(
      sex,
      "M" ~ "Male",
      "F" ~ "Female",
      .default = sex),
    # Make species, Sex, Release.site into factors? ASKED https://github.com/hugomflavio/actel/issues/130
    clasper_inner_length_cm = as.numeric(clasper_inner_length_cm),
    clasper_outer_length_cm = as.numeric(clasper_outer_length_cm),
    Mature = as.logical(dplyr::case_match(
      Mature,
      "Y" ~ TRUE,
      "N" ~ FALSE,
      .default = as.logical(NA))),
    pectoral_fin_girth_cm = as.numeric(pectoral_fin_girth_cm),
    dorsal_fin_girth_cm = as.numeric(dorsal_fin_girth_cm),
    weight_kg = as.numeric(weight_kg),
    # M: No Code.space column was found in the biometrics. Assigning code spaces based on detections
    # But called CodeSpace in online help?
    Code.space = as.integer(stringr::str_sub(string = acoustic_transmitter,
                                            start = 5,
                                            end = 8)),
    CodeSpaceSignal = paste(Code.space, acoustic_transmitter_ID, sep = "-"),
    acoustic_transmitter_ID = as.integer(acoustic_transmitter_ID),
    ID_tag_leader_number = as.integer(ID_tag_leader_number),
    ID_tag_side = as.character(ID_tag_side),
    # Correct column types when populated
    dorsal_fin_clip = as.logical(dplyr::case_match(
      dorsal_fin_clip,
      "Y" ~ TRUE,
      "N" ~ FALSE,
      .default = as.logical(NA))),
    mortality_expected = as.logical(dplyr::case_match(
      mortality_expected,
      "Y" ~ TRUE,
      "N" ~ FALSE,
      .default = as.logical(NA))),
    capture_depth_m = (capture_depth_ft * 0.3048)
  ) |>
  dplyr::rename(Release.site = tag_location,
                # release.site used in report
                # Does anything use release site/blank cells in other sheets?
                # Existing columns length, weight, mass: distribution graphics drawn per animal group in report.
                # length = Length.mm
                # Group = species,
                Latitude = latitude,
                Longitude = longitude,
                Sex = sex,
                Signal = acoustic_transmitter_ID
  ) |>
  # dplyr::filter(Group == "Bull") |> # remove non bulls for now
  tidyr::drop_na(Signal) |>
  dplyr::select(
    Release.date,
    date_encountered,
    time_tagged,
    Release.site,
    Latitude,
    Longitude,
    capture_method,
    capture_depth_ft,
    capture_depth_m,
    Group,
    Sex:acoustic_transmitter,
    Code.space,
    Signal,
    CodeSpaceSignal:Notes
  )
dir.create(file.path(loadloc, "actel", "processed"))
saveRDS(object = biometrics, file = file.path(loadloc, "actel", "processed", "biometrics.Rds"))









## 2. Spatial file ####

# Left_join station_log to ours & fact files everywhere
spatial <- readr::read_csv(file = file.path(loadloc, "actel", "Station_log.csv")) |>
  # dplyr::mutate(
    # Station.name = dplyr::case_when(
    #   "Cross Current Barge" ~ "CCBrge",
    #   "Deep Ledge" ~ "DpLedg",
    #   "Jupiter" ~ "Jupitr",
    #   "Princess Anne" ~ "PrncsA",
    #   "Rubble Pile" ~ "RublPl",
    #   "St Jaques" ~ "StJaqs",
    #   .default = Station.name
    # )
  # )
  dplyr::select(-Station.name) |>
  dplyr::rename(Station.name = Station.name.clean)

# # Remove this whole block, obsoleted by Station_log.csv
# spatial <- readr::read_csv(file.path(loadloc, "actel", "spatial.csv")) |>
#   dplyr::mutate(Station.name = stringr::str_to_title(Station.name),
#                 Station.name = dplyr::case_match(
#                   Station.name,
#                   "Mg111" ~ "MG111",
#                   .default = Station.name)) |>
#   # if spatial.csv obviated by stationLog, don't need factSpatial
#   dplyr::bind_rows(factSpatial) |>
#   # Use distinct, ungroup, is obviated by left_join?
# dplyr::group_by(Station.name) |> # dedupe stations, removes MG111 dupe
#   dplyr::summarise_all(dplyr::first) |>
#   # remove stations with no deployments (possibly due to not being downloaded thus removed)
#   # Govenors wrecks, deep ledge, jupiter: all release sites (what's the point of release sites then?)
#   dplyr::filter(Station.name %in% deployments$Station.name) |>
#   dplyr::ungroup()
saveRDS(object = spatial, file = file.path(loadloc, "actel", "processed", "spatial.Rds"))
spatial <- readRDS(file = file.path(loadloc, "actel", "processed", "spatial.Rds"))

# Error: The following station is listed in the spatial file but no receivers were ever deployed there: 'Deep Ledge'
# dplyr::filter(Station.name != "Deep Ledge")
# St Jaques, Governors Wrecks, 1, all same latlon
# rm(factSpatial)



# TODO? make release site name = nearest receiver? ####
# Dupe names should get solved by merging Station.name to receiver ID above


## gbm.auto basemap & raster ####
# install.packages("gbm.auto")
library(gbm.auto)
# create shapefile and respective auxiliary files in the folder where you have your 'spatial.csv'.
dir.create(file.path(loadloc, "actel", "NOAAcoastlines"))
basemap <- gbm.auto::gbm.basemap(bounds = c(min(spatial$Longitude),
                                            max(spatial$Longitude),
                                            min(spatial$Latitude),
                                            max(spatial$Latitude)),
                                 savedir = file.path(loadloc, "actel", "NOAAcoastlines"),
                                 extrabounds = TRUE)
# convert to raster
base.raster <- actel::shapeToRaster(shape = file.path(loadloc, "actel", "NOAAcoastlines", "CroppedMap", "Crop_Map.shp"), # path to your shapefile, including the ".shp" extension
                                    size = 0.01, # of the raster pixels in metres (or possibly 100km's)
                                    spatial = spatial, # " character string specifying the path to a spatial.csv file or a spatial data frame
                                    coord.x = "Longitude", # names of the columns containing the x and y coordinates
                                    coord.y = "Latitude",
                                    buffer = NULL) # request an expansion of the shapefile limits
saveRDS(object = base.raster, file = file.path(loadloc, "actel", "processed", "base.raster.Rds"))
# base.raster <- readRDS(file = file.path(loadloc, "actel", "processed", "base.raster.Rds"))

# could use gbm.auto code to do this easily if Hugo wants, depending on how it goes for me.
raster::plot(base.raster)
points(x = spatial$Longitude,
       y = spatial$Latitude,
       pch = 20,
       col = "red")

## Create transition layer ####
t.layer <- actel::transitionLayer(base.raster,
                                  directions = 16)
# shapeToRaster$size = 0.001 crashed Rstudio
saveRDS(object = t.layer, file = file.path(loadloc, "actel", "processed", "t.layer.Rds"))
# t.layer <- readRDS(file = file.path(loadloc, "actel", "processed", "t.layer.Rds"))

## 3. Distances file ####
# https://hugomflavio.github.io/actel-website/manual-distances.html
# create a shapefile that extends over all your receivers and release sites
distances <- actel::distancesMatrix(
  t.layer = t.layer,
  starters = spatial,
  targets = spatial,
  coord.x = "Longitude",
  coord.y = "Latitude",
  # id.col = "Station.name",
  actel = FALSE
)
# Colnames & rownames don't match due to Xs
# bug on actel: https://github.com/hugomflavio/actel/issues/129
# spatial <- spatial |> dplyr::mutate(Station.name = stringr::str_replace_all(string = Station.name, pattern = " ", replacement = "_"))
colnames(distances) <- spatial$Station.name
rownames(distances) <- spatial$Station.name
saveRDS(object = distances, file = file.path(loadloc, "actel", "processed", "distances.Rds"))






## 4. Detections file ####

### FWCdepredationStudy file ####
# Archive FWCdepredationStudy-Detections.csv & comment out this section since it's not used
# detections <- readr::read_csv(file.path(loadloc, "FWCdepredationStudy-Detections.csv")) |>
#   dplyr::select(!dplyr::where(~all(is.na(.)))) |>
#   dplyr::mutate(Signal = as.integer(stringr::str_sub(string = CodeSpace,
#                                                      start = 10,
#                                                      end = -1)),
#                 CodeSpace = stringr::str_sub(string = CodeSpace,
#                                              start = 5,
#                                              end = 8),
#                 Receiver = as.integer(Receiver),
#                 Station.name = dplyr::case_match(
#                   Station.name,
#                   "Mg111" ~ "MG111",
#                   .default = Station.name),
#                 Timestamp = as.POSIXct(Timestamp, format = "%d/%m/%Y %H:%M", tz = "America/New_York"),
#                 Timestamp = lubridate::with_tz(Timestamp, tzone = "UTC") # change to UTC
#                 # doesn't have seconds so won't match on seconds.
#   )


### Covert compile VRL files ####
# Needs Vemco VUE in Windows
# https://stackoverflow.com/questions/67745382/vemco-acoustic-telemetry-data-vrl-files-in-r
# remotes::install_github("ocean-tracking-network/glatos")
# library(glatos)
# csvfiles <- glatos::vrl2csv(vrl = file.path(loadloc, "VRL/"),
#                             outDir = file.path(loadloc, "VRL/"),
#                             vueExePath = "C:/Program Files (x86)/VEMCO VUE")

# vrl2csv doesn't correct for clock drift. In Vue: time-correct each file using the VRL editor in VUE (under Tools menu).
# uncheck Import checkbox next to each filename, then run `vrl2csv` to create a CSV for each edited (e.g. time-corrected) VRL.

# Em has a tool in R

# TODO TARGETS only convert new VRLs ####
# Query against deployment log for deployments$Filename
# Need a way to keep track of which filenames have been done - see blocklab process
# Use targets(), see bookmarks

# csvfiles <- list.files(path = file.path(loadloc, "VRL"),
#                        pattern = "^VR2W_.*csv$", # ^starts with (VR2W_), .* unbounded sequence of characters, $ ends with csv
#                        # avoids importing RLD files which are blank
#                        full.names = TRUE)


detections <- read.csv(file = file.path(loadloc, "actel", "detections", "detection-data_FWC-bull-Shark_20240923_01.csv")) |>
  # do.call(rbind, lapply(csvfiles, read.csv)) |> # code to pull csvfiles
  # remove NA columns: transmitter name, transmitter serial, station name, lat, lon, transmitter type, sensor precision.
  dplyr::select(!dplyr::where(~all(is.na(.)))) |>
  dplyr::mutate(Timestamp = as.POSIXct(Date.and.Time..UTC.,
                                       format = "%Y-%m-%d %H:%M:%S", # all bar VR2W_139045_20240923_1.csv work
                                       tz = "UTC"),  # "America/New_York"
                Signal = as.integer(stringr::str_sub(string = Transmitter,
                                                     start = 10,
                                                     end = -1)),
                CodeSpace = as.integer(stringr::str_sub(string = Transmitter,
                                                        start = 5,
                                                        end = 8)),
                CodeSpaceSignal = paste(CodeSpace, Signal, sep = "-"),
                Receiver = as.integer(stringr::str_sub(string = Receiver,
                                                       start = 6,
                                                       end = -1))) |>
  tidyr::drop_na(Signal) |>
  dplyr::select(Timestamp, Receiver, CodeSpace, Signal, CodeSpaceSignal, Sensor.Value, Sensor.Unit)
# rm(csvfiles)

# check overlap of old detections with new AllDetections
# Doesn't match because AllDetections has seconds and detections doesn't:
# AllDetectionsTmp <- AllDetections |> dplyr::mutate(Timestamp = lubridate::floor_date(Timestamp, unit = "minutes"))
# oldNotInNew <- detections[which(!detections$Timestamp %in% AllDetectionsTmp$Timestamp),] #|>
# 2024-10-15: 0.
# Since there are more data in AllDetections (VRL files) than detections (FWCdepredationStudy-Detections.csv), ignore the latter
# rm(detections)
# detections <- AllDetections
# rm(AllDetections)
# rm(AllDetectionsTmp)
# rm(oldNotInNew)

# Create/import lookup table of receivers, depthM, substrate, array, section
# unique from deployments
# deployments |>
#   dplyr::select(Receiver, Latitude, Longitude, ReceiverDepthM, Substrate, Array, Section) |>
#   dplyr::group_by(Receiver) |>
#   dplyr::summarise(Latitude = mean(Latitude, na.rm = TRUE),
#                    Longitude = mean(Longitude, na.rm = TRUE),
#                    ReceiverDepthM = mean(ReceiverDepthM, na.rm = TRUE),
#                    Substrate = dplyr::first(Substrate, na_rm = TRUE),
#                    Array = dplyr::first(Array, na_rm = TRUE),
#                    Section = dplyr::first(Section, na_rm = TRUE)) |>
#   dplyr::ungroup()


### Read FACT detections ####
factfiles <- list.files(path = file.path(loadloc, "FACT"),
                        pattern = ".csv",
                        full.names = TRUE)

library(magrittr)

# tmp <- factDetections |>
#   group_by(Receiver) |>
#   summarise(nStation.name = length(unique(Station.name)),
#             nStart = length(unique(Start)),
#             nStop = length(unique(Stop)),
#             nlat = length(unique(Latitude)),
#             nlon = length(unique(Longitude)),
#             nreceiver_depth = length(unique(receiver_depth)),
#             nSection = length(unique(Section))
#             ) # all 1s, thus can group on Receiver only for factDeployments
# # 136423 & 136424 have 2 stops. Use distinct.

factDetections <- do.call(rbind, lapply(factfiles, read.csv)) |> # read & bind files
  dplyr::select(!dplyr::where(~all(is.na(.)))) |> # remove blank columns
  dplyr::rename(CodeSpace = codespace, # rename columns
                Receiver = receiver,
                Station.name = station,
                Latitude = latitude,
                Longitude = longitude) |>
  dplyr::filter(Receiver != "release", # Removes release receiver sites, removes 15 rows, allows integer
                receiver_group != "FIUBULL") |> # Removes hits from our own receivers which are already in detections
  dplyr::mutate(
    Timestamp = as.POSIXct(x = paste(paste(yearcollected, # create timestamp from correct data from columns
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
    # create 'max 6 character' species/Group alternative
    # Group = dplyr::case_match(
    #   species,
    #   "Caribbean Reef" ~ "Crb.Rf",
    #   "Sandbar" ~ "Sndbar",
    #   .default = species), # "Bull"
    # 2024-11-06 currently only has bull shark so no need for this yet.
    Signal = as.numeric(stringr::str_sub(string = tagname,
                                         start = 10,
                                         end = -1)),
    CodeSpace = as.integer(stringr::str_sub(string = CodeSpace,
                                            start = 5,
                                            end = 8)),
    CodeSpaceSignal = paste(CodeSpace, Signal, sep = "-"),
    Sensor.Unit = as.character(NA),
    # # = Emily "Region", typically county. Section = "Ocean", # for spatial
    # # = Emily "Location", typically beach site or city. Array = "A1", # for spatial
    # Type = "Hydrophone" # for spatial
  ) %T>% # spit out files an intermediary steps
  # 2424 before, 2424 after. FACT only send us our sharks.
  {. |> dplyr::arrange(Receiver, Stop) |> # FACT deployments file
      dplyr::mutate(Substrate = NA_character_,
                    Array = NA_character_) |>
      dplyr::rename(ReceiverDepthM = receiver_depth) |>
      dplyr::select(Station.name, Receiver, Start, Stop, Latitude, Longitude, ReceiverDepthM, Substrate) |> # , Array, Section
      dplyr::distinct(.keep_all = TRUE) ->> factDeployments} %T>% # export changes to deployments
  {. |> dplyr::select(Station.name, Latitude, Longitude) |> # FACT spatial file # , Section, Array, Type
      dplyr::arrange(Station.name) |>
      dplyr::group_by(Station.name, Latitude, Longitude) |>
      dplyr::summarise_all(dplyr::first) ->> factSpatial} |>
  dplyr::select(Timestamp, Receiver, CodeSpace, Signal, CodeSpaceSignal, Sensor.Unit, Group) # select only columns needed
rm(factfiles)
# Section Array Type removed from here, to be left_joined from station_log/spatial

# check overlap of factDetections with detections. Both have seconds
# oldNotInNew <- factDetections[which(!factDetections$Timestamp %in% detections$Timestamp),] #|>
# 2024-10-15: 2424 i.e. all factDetections.
# rm(oldNotInNew)
# tmp <- dplyr::bind_rows(detections |> dplyr::mutate(Source = "detections"),
#                         factDetections |> dplyr::mutate(Source = "factDetections"))
# rm(tmp)
detections <- detections |>
  dplyr::bind_rows(factDetections) %T>% # 1534 + 2424 = 3958
  {saveRDS(object = .,file = file.path(loadloc,
                                       "actel",
                                       paste0("detections_FWCbullShark_",
                                              as.Date(max(.$Timestamp, na.rm = TRUE)))))} |>
  # Filter out non-Yannis sharks (Signal)
  dplyr::filter(CodeSpaceSignal %in% unique(biometrics$CodeSpaceSignal))
# plot(x = tmp$Timestamp, y = as.factor(tmp$Receiver))
rm(factDetections)
saveRDS(object = detections, file = file.path(loadloc, "actel", "processed", "detections.Rds"))






## 5a. Deployments file: our csv ####
deployments <- readr::read_csv(file.path(loadloc, "actel",  "deployments.csv")) |>
  dplyr::rename(Station.name = Station,
                Receiver = "Receiver SN",
                Start = In_ESTEDT,
                Stop = Out_ESTEDT,
                Section = Region,
                Array = Location) |>
  dplyr::mutate(
    Station.name = stringr::str_to_title(Station.name),
    Start = lubridate::force_tz(Start, tzone = "America/New_York"), # import automatically converts to POSIX date and assumes tz=UTC. Force override.
    Stop = lubridate::force_tz(Stop, tzone = "America/New_York"), #
    Start = lubridate::with_tz(Start, tzone = "UTC"), # # timeshift to UTC
    Stop = lubridate::with_tz(Stop, tzone = "UTC"), # change to UTC
    Station.name = dplyr::case_match(
      Station.name,
      "Mg111" ~ "MG111",
      .default = Station.name),
    ReceiverDepthM = (ReceiverDepthFt * 0.3048)
  ) |>
  tidyr::drop_na(Start, Stop) # remove undownloaded receivers as they crash preload




## 5b. Deployments file: join FACT ####

# overwrite start dates for stations with 2+ entries
# Station 23 & 24, Receiver 136423 & 136424, have different stops each but same starts.
# factDeployments |>
#   dplyr::group_by(Station.name) |>
#   dplyr::summarise(n = dplyr::n(),
#                    Start = dplyr::first(Stop)) |>
#   dplyr::filter(n > 1) |>
#   dplyr::select(-n)
factDeployments$Start[which(duplicated(x = factDeployments$Receiver))] <- factDeployments$Stop[which(duplicated(x = factDeployments$Receiver)) - 1]
# Populate the duplicated (second) start row with the first (row-1) stop row date


# join ours with FACT
deployments <- deployments |>
  # dplyr::select(-Notes) |>
  dplyr::bind_rows(factDeployments) |> # join ours with FACT
  # Left_join station log details
  dplyr::select(-ReceiverDepthM, -Substrate, -Array, -Section) |>
  dplyr::left_join(spatial |>
                     dplyr::select(Station.name.clean, ReceiverDepthM, Substrate, Section, Array, Type) |>
                     dplyr::rename(Station.name = Station.name.clean) |>
                     dplyr::distinct(Station.name, .keep_all = TRUE),
                   by = "Station.name") |>
  # factor order Section & Array by (average) Latitude
  dplyr::arrange(desc(Latitude)) |>
  dplyr::mutate(
    Section = factor(Section, levels = (unique(Section))),
    Array = factor(Array, levels = (unique(Array)))
  ) |>
  dplyr::arrange(Receiver) |>
  # All group by receiver depth later <60, 60-80, >80 ft = 18.288 & 24.384 m
  dplyr::mutate(ReceiverDepthBin = cut(ReceiverDepthM,
                                       c(-Inf, 18.288, 24.384, Inf),
                                       labels = c("0-18.288", "18.288-24.384m", "24.384m+"))) # 2024-10-21: n = 16, 14, 11 per bin
rm(factDeployments)

# tmp <- deployments |>
#   # dplyr::group_by(Receiver) |>
#   # dplyr::summarise(nStation.name = length(unique(Station.name)),
#   #           nStart = length(unique(Start)),
#   #           nStop = length(unique(Stop)),
#   #           nlat = length(unique(Latitude)),
#   #           nlon = length(unique(Longitude)),
#   #           nReceiverDepthM = length(unique(ReceiverDepthM)),
#   #           nSection = length(unique(Section))
#   #           ) |>
#   # dplyr::ungroup() |>
#   # dplyr::filter(nStart > 1) # |> # tried dplyr::filter(dplyr::if_any(dplyr::across(nStation.name:nSection), ~ .x > 1)) # but no luck
#   # dplyr::pull(Receiver) # 136423 136424 138239 138240 487189 # 5 with multiple Starts & Stops, some have multiple depths, Sections, Lats, Lons, Station.names.
#   dplyr::select(Station.name, Receiver, Start, Stop, Latitude, Longitude, ReceiverDepthM, Substrate, Array, Section) |>
#   dplyr::distinct() |>
#   dplyr::arrange(Receiver, Stop) %T>%
#   write.csv(file.path(loadloc, "actel", "AllDeployments.csv"), row.names = FALSE)

# write.csv(x = deployments,
#           file = file.path(loadloc, "actel", "processed", "deployments_toFix.csv"),
#           row.names = FALSE)

# deployments[deployments$Receiver == 138239 & deployments$Station.name == "3", "Start"] <- deployments[deployments$Receiver == 138239 & deployments$Station.name == "Princess Anne", "Stop"] # Station.name now gone
# deployments[deployments$Receiver == 138240 & deployments$Station.name == "1", "Start"] <- deployments[deployments$Receiver == 138240 & deployments$Station.name == "St Jaques", "Stop"] # Station.name now gone

# princess Anne / 3: no date overlap. Should fix name?
# St Jaques / 1 / MG111: No overlap first 2, 1/MG111 date overlap 4 days. Should fix name?
# deployments[deployments$Receiver == 139043 & deployments$Station.name == "Princess Anne", "Start"] <- deployments[deployments$Receiver == 138239 & deployments$Station.name == "Princess Anne", "Stop"] # Already in correct state
# 2nd receiver deployed while first still present, check, prefixed, remove
# deployments[deployments$Receiver == 138240 & deployments$Station.name == "MG111", "Start"] <- deployments[deployments$Receiver == 138240 & deployments$Station.name == "1", "Stop"]
# tmp <- deployments |> dplyr::mutate(timediff = Stop - Start) # check if any deployments stop before they start

# Do we have a download date?
# tmp <- factDetections |> group_by(receiver_group, station) |> summarise(n = dplyr::n(), contact_pi = dplyr::first(contact_pi))
# write.csv(x = tmp,file = file.path(loadloc, "FactStations.csv"), row.names = FALSE)

# use start & end time & receiver ID to left_join/overwrite Station.name for FACT deployments
# Obviated by left_join earlier up
# Em code:
# live_buoy_detections <- live_buoy_detections %>%
#   inner_join(lb_log, by = "Receiver") %>%
#   filter(DateTimePT >= In_TimePT & DateTimePT <= Out_TimePT)
saveRDS(object = deployments, file = file.path(loadloc, "actel", "processed", "deployments.Rds"))





# Preload ####
# Load objects
biometrics <- readRDS(file = file.path(loadloc, "actel", "processed", "biometrics.Rds"))
spatial <- readRDS(file = file.path(loadloc, "actel", "processed", "spatial.Rds"))
deployments <- readRDS(file = file.path(loadloc, "actel", "processed", "deployments.Rds"))
detections <- readRDS(file = file.path(loadloc, "actel", "processed", "detections.Rds"))
distances <- readRDS(file = file.path(loadloc, "actel", "processed", "distances.Rds"))

mypreload <- actel::preload(biometrics = biometrics,
                            spatial = spatial,
                            deployments = deployments,
                            detections = detections,
                            distances = distances,
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

# 2024-10-15
# M: Preloaded Release dates are already in POSIX format. Skipping timestamp format checks.
# M: No Code.space column was found in the biometrics. Assigning code spaces based on detections.
# Warning: Long group names detected. To improve graphic rendering, consider keeping group names under six characters.
# M: Number of target tags: 18.
# M: Preloaded deployment times are already in POSIX format. Skipping timestamp format checks.
# Error: Receiver 138240 was re-deployed before being retrieved:
# 6    St Jaques   138240          NA                 NA 2022-06-29 2023-07-14          success            NA 26.75152 -80.01010           VR2W               NA VR2W_138240_20230718_1.vrl              90
# 8            1   138240          NA                 NA 2023-07-14 2023-10-10             <NA>            NA       NA        NA           <NA>               NA                       <NA>              NA
# 2        MG111   138240          NA                 NA 2023-10-06 2024-07-31          success            NA 26.97735 -80.02477           VR2W               NA VR2W_138240_20240801_1.vrl              65
# Mooring Components
# 6 poly pro spliced onto wreck (5), foam float (1)
# 8                                            <NA>
# 2                                            <NA>  <- !!!
# Error: Fatal exception found. Read lines above for more details.

# 2024-10-15
# Force fixed that above.
# Error in if (any(link <- input$Start > input$Stop)) { : missing value where TRUE/FALSE needed

# Error: Receiver 138239 was re-deployed before being retrieved:
# 3  Princess Anne   138239          NA                 NA 2022-05-05 00:00:00 2023-05-25          success            NA 26.79355 -80.00385           VR2W               NA VR2W_138239_20230526_1.vrl              90 poly pro spliced onto wreck (5), foam float (1)
# 14             3   138239          NA                 NA 2022-05-07 15:30:02 2023-10-10             <NA>            NA       NA        NA           <NA>               NA                       <NA>              NA                                            <NA>  <- !!!
# Force fixed.

# Error: Some deployment periods end before they have started! Please fix this before continuing. Troublesome rows: 4
# Force fixed.

# Error: The 'Station.name' column in the spatial input must not have duplicated values. Stations appearing more than once: MG111


# M: Preloaded Release dates are already in POSIX format. Skipping timestamp format checks.
# M: No Code.space column was found in the biometrics. Assigning code spaces based on detections.
# Warning: Long group names detected. To improve graphic rendering, consider keeping group names under six characters.
# M: Number of target tags: 18.
# M: Preloaded deployment times are already in POSIX format. Skipping timestamp format checks.
# Warning: The 'Signal' column in the detections is not of type integer. Attempting to convert.
# Warning: The 'Receiver' column in the detections is not of type integer. Attempting to convert.
# Warning: Release sites were not specified in the spatial.csv file. Attempting to assume all released animals start at the top level array.
# M: Matching detections with deployment periods.
# Error: 18 detections for receiver 134302 do not fall within deployment periods.
#
# Timestamp Receiver CodeSpace Signal Sensor.Value Sensor.Unit  Group
# <POSc>   <char>    <char>  <int>        <int>      <char> <char>
# 1: 2023-08-28 01:08:12   134302  A69-9001  57458           NA        <NA>   Bull
# 2: 2023-08-28 01:09:11   134302  A69-9001  57458           NA        <NA>   Bull
# 3: 2023-08-28 01:10:48   134302  A69-9001  57458           NA        <NA>   Bull
# 4: 2023-08-28 01:13:39   134302  A69-9001  57458           NA        <NA>   Bull
# 5: 2023-08-28 12:28:22   134302  A69-9001  57458           NA        <NA>   Bull
# 6: 2023-08-28 12:29:12   134302  A69-9001  57458           NA        <NA>   Bull
# 7: 2023-08-28 12:29:51   134302  A69-9001  57458           NA        <NA>   Bull
# 8: 2023-08-28 12:31:13   134302  A69-9001  57458           NA        <NA>   Bull
# 9: 2023-08-28 12:31:58   134302  A69-9001  57458           NA        <NA>   Bull
# 10: 2023-08-28 12:33:33   134302  A69-9001  57458           NA        <NA>   Bull
# 11: 2023-08-28 12:39:20   134302  A69-9001  57458           NA        <NA>   Bull
# 12: 2023-08-29 00:32:42   134302  A69-9001  57458           NA        <NA>   Bull
# 13: 2023-09-08 04:24:20   134302  A69-9001  57458           NA        <NA>   Bull
# 14: 2023-09-08 04:27:39   134302  A69-9001  57458           NA        <NA>   Bull
# 15: 2023-09-08 04:28:36   134302  A69-9001  57458           NA        <NA>   Bull
# 16: 2023-09-08 04:30:10   134302  A69-9001  57458           NA        <NA>   Bull
# 17: 2023-09-08 04:30:54   134302  A69-9001  57458           NA        <NA>   Bull
# 18: 2023-09-08 04:31:45   134302  A69-9001  57458           NA        <NA>   Bull
#
# Possible options:
# a) Stop and double-check the data (recommended)
# b) Discard orphan detections in this instance.
# c) Discard orphan detections for all instances.
# d) Save orphan detections to a file and re-open dialogue.
# C

# Error: 100 detections for receiver 138240 do not fall within deployment periods. Discarding orphan detections.
# Error: 12 detections for receiver 484731 do not fall within deployment periods. Discarding orphan detections.
# Error: 31 detections for receiver 484823 do not fall within deployment periods. Discarding orphan detections.

# Warning: The column and row names in the distances matrix do not match each other. Deactivating speed calculations to avoid function failure.
# Row names missing in the columns: '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43', '44', '45'.
# Column names missing in the rows: 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31', 'X32', 'X33', 'X34', 'X35', 'X36', 'X37', 'X38', 'X39', 'X40', 'X41', 'X42', 'X43', 'X44', 'X45'.

# Stray tags were detected in your study area. Would you like to save a summary to stray_tags.csv?(y/n) y

# Warning: Tag A69-9001-57458 was detected before being released!
# Release time: 2023-06-29 11:36:00
# First detection time: 2023-06-28 20:32:56
# Number of detections before release: 31
#
# Possible options:
# a) Stop and double-check the data (recommended)
# b) Discard orphan detections in this instance.
# c) Discard orphan detections for all instances.
# d) Save orphan detections to a file and re-open dialogue.

# M: 31 detection(s) from tag A69-9001-57458 removed per user command.
# M: Data successfully imported!



# 2024-10-06
# M: Preloaded Release dates are already in POSIX format. Skipping timestamp format checks.
# M: No Code.space column was found in the biometrics. Assigning code spaces based on detections.
# Warning: Long group names detected. To improve graphic rendering, consider keeping group names under six characters.
# M: Number of target tags: 23.
# M: Preloaded deployment times are already in POSIX format. Skipping timestamp format checks.
# Error: The 'Station.name' column in the spatial input must not have duplicated values.
# Stations appearing more than once: GNWR, HRFN






# Summary & Explore ####
# Get summary of data
# install.packages("gWidgets2tcltk")
library(gWidgets2tcltk)
# dir.create(file.path(loadloc, "actel", "explore"))
setwd(file.path(loadloc, "actel", "explore"))
exploreResults <- actel::explore(datapack = mypreload,
                                 tz = NULL, # tz = "America/New_York", not needed if datapack provided
                                 max.interval = 60, # number of minutes that must pass between detections for a new event to be created
                                 min.total.detections = 2,
                                 min.per.event = 1,
                                 start.time = NULL, # Detection data prior to the timestamp set in start.time (in YYYY-MM-DD HH:MM:SS format) is not considered during the analysis
                                 stop.time = NULL, # ditto, posterior
                                 speed.method = c("last to first", "last to last", "first to first"),
                                 # And do in other functions below
                                 speed.warning = 1, # NULL; warns if speed in m/s > this value. <= speed.error. See Yannis email 2024-10-01 "Bull shark data cleaning questions"
                                 speed.error = 3, # ditto but suggests user input
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
                                 # HUGO NOTE: manpage unclear why user would want this
                                 # https://github.com/hugomflavio/actel/issues/111
                                 GUI = c("needed", "always", "never"),
                                 save.tables.locally = FALSE, # only if too big for R console/whenever possibility to invalidate events/never
                                 print.releases = TRUE, # print release sites on study area diagrams
                                 detections.y.axis = c("auto", "stations", "arrays")) # for individual detection plots

# Warning: 'speed.warning'/'speed.error' were not set, skipping speed checks.
# Warning: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.
# Warning: Tag A69-9001-52506 (3/11) has fewer than 2 detections in total. Discarding this tag.
# Warning: Tag A69-9001-52511 (5/11) has fewer than 2 detections in total. Discarding this tag.





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
  speed.warning = 1,
  speed.error = 3,
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
# Warning: 'success.arrays' was not defined. Assuming success if the tags are last detected at array A1.
# Warning: 'speed.warning'/'speed.error' were not set, skipping speed checks.
# Warning: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.
# Warning: Tag A69-9001-52506 (3/11) has fewer than 2 detections in total. Discarding this tag.
# Warning: Tag A69-9001-52511 (5/11) has fewer than 2 detections in total. Discarding this tag.
# Warning: None of the arrays has valid efficiency peers.
# Warning: Aborting inter-array efficiency calculations (will limit the report's output).




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
  speed.warning = 1,
  speed.error = 3,
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

# Warning: 'speed.warning'/'speed.error' were not set, skipping speed checks.
# Warning: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.
# Warning: Tag A69-9001-52506 (3/11) has fewer than 2 detections in total. Discarding this tag.
# Warning: Tag A69-9001-52511 (5/11) has fewer than 2 detections in total. Discarding this tag.


# TODO list ####
# 2024-11-06
# Error: The 'Station.name' column in the spatial input must not have duplicated values.
# Await YP reply about Stations 1 & 3 (FACT versions of StJ & PrncsA) depth m/ft error, dupe removal


# manually map function scripts pulled from this file,
# check (circular) dependencies,
# use dagitty
# once resolved, write function scripts (should be quick hopefully)
# convert to targets/write targets script(s) etc.
# have runscript (last-ish in chain) as a Quarto markdown doc,
# which saves its HTML results locally,
# But also serves them in-frame on the local instance of the rendered markdown,
# (cloud-hosted with a data cap would enable this to be run standalone in-page by others)
# Could therefore tweak the runcode in the markdown higher up on the page before results are presented
# Thus tuning models online.
# This page could be a full dashboard of results options; see semi-recent Dancho lesson I tried [SD note to self]
# Host on github.io for this project

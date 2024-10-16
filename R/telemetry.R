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

# remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
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

## Biometrics file ####
# Release.date: date & time animal released, yyyy-mm-dd hh:mm:ss.
## Timestamps must be local tz study area, supply in tz argument.
# Signal: code emitted by tags. Ask tag manufacturer about differences between code spaces and signals.
# All animals need a Signal value else function crashes; flag & remove NAs

# print(paste0(length(which(is.na(biometrics$Signal))), " animals missing Signal values, which will be removed"))
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
  dplyr::rename(Station.name = Station,
                Receiver = "Receiver SN",
                Start = In_ESTEDT,
                Stop = Out_ESTEDT) |>
  dplyr::mutate(Station.name = stringr::str_to_title(Station.name),
                Start = as.POSIXct(Start, format = "%d/%m/%Y %H:%M"),
                Stop = as.POSIXct(Stop, format = "%d/%m/%Y %H:%M"),
                Station.name = dplyr::case_match(
                  Station.name,
                  "Mg111" ~ "MG111",
                  .default = Station.name)) |>
  tidyr::drop_na(Start, Stop) # remove undownloaded receivers as they crash preload





## Detections file ####
### FWCdepredationStudy file ####
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

### Covert compile VRL files ####
# Needs Vemco VUE in Windows
# https://stackoverflow.com/questions/67745382/vemco-acoustic-telemetry-data-vrl-files-in-r
# remotes::install_github("ocean-tracking-network/glatos")
# library(glatos)
# csvfiles <- glatos::vrl2csv(vrl = file.path(loadloc, "VRL/"),
#                             outDir = file.path(loadloc, "VRL/"),
#                             vueExePath = "C:/Program Files (x86)/VEMCO VUE")
csvfiles <- list.files(path = file.path(loadloc, "VRL"),
                       pattern = ".csv",
                       full.names = TRUE)
# remove blank files from list, don't import
csvfiles <- csvfiles[c(1:7)]

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
rm(csvfiles)

# check overlap of old detections with new AllDetections
# Doesn't match because AllDetections has seconds and detections doesn't:
# AllDetectionsTmp <- AllDetections |> dplyr::mutate(Timestamp = lubridate::floor_date(Timestamp, unit = "minutes"))
# oldNotInNew <- detections[which(!detections$Timestamp %in% AllDetectionsTmp$Timestamp),] #|>
# 2024-10-15: 0.
# Since there are more data in AllDetections (VRL files) than detections (FWCdepredationStudy-Detections.csv), ignore the latter
rm(detections)
detections <- AllDetections
rm(AllDetections)
# rm(AllDetectionsTmp)
# rm(oldNotInNew)



### Read FACT detections ####
factfiles <- list.files(path = file.path(loadloc, "FACT"),
                        pattern = ".csv",
                        full.names = TRUE)

library(magrittr)
factDetections <- do.call(rbind, lapply(factfiles, read.csv)) |> # read & bind files
  dplyr::select(!dplyr::where(~all(is.na(.)))) |> # remove blank columns
  dplyr::rename(CodeSpace = codespace, # rename columns
                Receiver = receiver,
                Station.name = station,
                Latitude = latitude,
                Longitude = longitude) |>
  dplyr::filter(Receiver != "release") |> # Removes release receiver sites, removes 15 rows, allows integer
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
    Signal = as.numeric(stringr::str_sub(string = tagname,
                                         start = 10,
                                         end = -1)),
    Sensor.Unit = as.character(NA),
    Section = "Ocean", # for spatial
    Array = "A1", # for spatial
    Type = "Hydrophone" # for spatial
  ) %T>% # spit out files an intermediary steps
  {. |> dplyr::arrange(Receiver) |> # FACT deployments file
      dplyr::group_by(Receiver, Station.name, Stop) |>
      dplyr::summarise_all(dplyr::first) |>
      dplyr::select(Receiver, Station.name, Start, Stop) |>
      dplyr::arrange(Station.name, Stop) ->> factDeployments} %T>% # export changes to deployments
  {. |> dplyr::select(Station.name, Latitude, Longitude, Section, Array, Type) |> # FACT spatial file
      dplyr::arrange(Station.name) |>
      dplyr::group_by(Station.name, Latitude, Longitude) |>
      dplyr::summarise_all(dplyr::first) ->> factSpatial} |>
  dplyr::select(Timestamp, Receiver, CodeSpace, Signal, Sensor.Unit, Group) # select only columns needed
rm(factfiles)

# check overlap of factDetections with detections. Both have seconds
# oldNotInNew <- factDetections[which(!factDetections$Timestamp %in% detections$Timestamp),] #|>
# 2024-10-15: 2424 i.e. all factDetections.
# rm(oldNotInNew)
# tmp <- dplyr::bind_rows(detections |> dplyr::mutate(Source = "detections"),
#                         factDetections |> dplyr::mutate(Source = "factDetections"))
# rm(tmp)
detections <- dplyr::bind_rows(detections, factDetections)
rm(factDetections)



## Deployments file part 2 ####

# overwrite start dates for stations with 2+ entries
# factDeployments |>
#   dplyr::group_by(Station.name) |>
#   dplyr::summarise(n = dplyr::n(),
#                    Start = dplyr::first(Stop)) |>
#   dplyr::filter(n > 1) |>
#   dplyr::select(-n)
factDeployments$Start[which(duplicated(x = factDeployments$Station.name))] <- factDeployments$Stop[which(duplicated(x = factDeployments$Station.name)) - 1]
# tmp <- detections[which(duplicated(detections)),]


# join ours with FACT
deployments <- deployments |>
  dplyr::select(-Notes) |>
  dplyr::bind_rows(factDeployments)
rm(factDeployments)
# TODO: merge FACT/FIU station dupes####
### Fix deployment dupes date overlap issues ####
deployments[deployments$Receiver == 138239 & deployments$Station.name == "3", "Start"] <- deployments[deployments$Receiver == 138239 & deployments$Station.name == "Princess Anne", "Stop"]
deployments[deployments$Receiver == 138240 & deployments$Station.name == "1", "Start"] <- deployments[deployments$Receiver == 138240 & deployments$Station.name == "St Jaques", "Stop"]
# princess Anne / 3: no date overlap. Should fix name?
# St Jaques / 1 / MG111: No overlap first 2, 1/MG111 date overlap 4 days. Should fix name?
# FORCEFIX, check this ####
deployments[deployments$Receiver == 139043 & deployments$Station.name == "Princess Anne", "Start"] <- deployments[deployments$Receiver == 138239 & deployments$Station.name == "Princess Anne", "Stop"]
deployments[deployments$Receiver == 138240 & deployments$Station.name == "MG111", "Start"] <- deployments[deployments$Receiver == 138240 & deployments$Station.name == "1", "Stop"]

# deployments[deployments$Station.name == "1", "Stop"] <- as.POSIXct("2023-10-06", format = "%Y-%m-%d", tz = "UTC") # deployments 1 change stop to 2023-10-06
# deployments[deployments$Station.name == "Princess Anne", "Stop"] <- as.POSIXct("2022-05-07", format = "%Y-%m-%d", tz = "UTC")

tmp <- deployments |> dplyr::mutate(timediff = Stop - Start)

# Do we have a download date?
# tmp <- factDetections |> group_by(receiver_group, station) |> summarise(n = dplyr::n(), contact_pi = dplyr::first(contact_pi))
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

#### Join spatial & FACT spatial ####
spatial <- dplyr::bind_rows(spatial |> dplyr::mutate(Source = "spatial"),
                            factSpatial |> dplyr::mutate(Source = "factSpatial")) |>
  dplyr::arrange(Latitude, Longitude, Station.name) |>
  dplyr::distinct(Latitude, Longitude, Station.name, .keep_all = TRUE) # 1 row from factSpatial added: MG111 with a slightly different location
# TODO LatLon Dupes ####
# LatLon dupe 1/Governors Wrecks/St Jaques
rm(factSpatial)

# remove stations with no deployments (possibly due to not being downloaded thus removed)
# Govenors wrecks, deep ledge, jupiter: all release sites (what's the point of release sites then?)
spatial <- spatial |> dplyr::filter(Station.name %in% deployments$Station.name)

# Remove stations with duplicated values
spatial <- spatial |>
  dplyr::filter(Source != "factSpatial") |> # this was MG111
  dplyr::select(!Source) # not being used for anything now

install.packages("gbm.auto")
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
# could use gbm.auto code to do this easily if Hugo wants, depending on how it goes for me.
raster::plot(base.raster)
points(x = spatial$Longitude,
       y = spatial$Latitude,
       pch = 20,
       col = "red")

### Create a transition layer ####
t.layer <- actel::transitionLayer(base.raster,
                                  directions = 16)
# shapeToRaster$size = 0.001 crashed Rstudio




# Create distance matrix ####
# https://hugomflavio.github.io/actel-website/manual-distances.html
# create a shapefile that extends over all your receivers and release sites
distances <- actel::distancesMatrix(
  t.layer = t.layer,
  starters = spatial,
  targets = spatial,
  coord.x = "Longitude",
  coord.y = "Latitude",
  # id.col = NULL,
  actel = FALSE
)


# Save objects ####
dir.create(file.path(loadloc, "actel", "processed"))
saveRDS(object = biometrics,
        file = file.path(loadloc, "actel", "processed", "biometrics.Rds"))
saveRDS(object = spatial,
        file = file.path(loadloc, "actel", "processed", "spatial.Rds"))
saveRDS(object = deployments,
        file = file.path(loadloc, "actel", "processed", "deployments.Rds"))
saveRDS(object = detections,
        file = file.path(loadloc, "actel", "processed", "detections.Rds"))
saveRDS(object = base.raster,
        file = file.path(loadloc, "actel", "processed", "base.raster.Rds"))
saveRDS(object = t.layer,
        file = file.path(loadloc, "actel", "processed", "t.layer.Rds"))
saveRDS(object = distances,
        file = file.path(loadloc, "actel", "processed", "distances.Rds"))
biometrics <- readRDS(file = file.path(loadloc, "actel", "processed", "biometrics.Rds"))
spatial <- readRDS(file = file.path(loadloc, "actel", "processed", "spatial.Rds"))
deployments <- readRDS(file = file.path(loadloc, "actel", "processed", "deployments.Rds"))
detections <- readRDS(file = file.path(loadloc, "actel", "processed", "detections.Rds"))
base.raster <- readRDS(file = file.path(loadloc, "actel", "processed", "base.raster.Rds"))
t.layer <- readRDS(file = file.path(loadloc, "actel", "processed", "t.layer.Rds"))
distances <- readRDS(file = file.path(loadloc, "actel", "processed", "distances.Rds"))



# Preload ####
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

# Warning: 'speed.warning'/'speed.error' were not set, skipping speed checks.
# Warning: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.
# Warning: Tag A69-9001-52506 (3/11) has fewer than 2 detections in total. Discarding this tag.
# Warning: Tag A69-9001-52511 (5/11) has fewer than 2 detections in total. Discarding this tag.



# TODO ####
# All stations being Array==A1 & Section==Ocean disallows actel from doing much useful. Change in spatial

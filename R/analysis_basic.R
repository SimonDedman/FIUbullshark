# Simon Dedman, simondedman@gmail.com, 2025-02-10
# Inevitably also including Spurg, to add

# Load packages & data ####
library(tidyverse)
library(tidylog)
# First run telemetry_refactored.R to generate cleaned input files
# Open Rproject FIUbullshark
loadloc <- "/home/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data" # si linux
# loadloc <- "C:/Users/simon/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Data" # si windows
# loadloc <- "C:/Users/davon/OneDrive - Florida International University/FIU Bull Sharks/DATA" # davon
detections <- readRDS(file.path(loadloc, "actel", "processed", "detections.Rds"))
deployments <- readRDS(file.path(loadloc, "actel", "processed", "deployments.Rds"))
biometrics <- readRDS(file.path(loadloc, "actel", "processed", "biometrics.Rds"))

# Join data to detections ####
# deployments: left_join Receiver & Timestamp between Start & Stop
detections <- detections |>
  dplyr::select(-Sensor.Value,
                -Sensor.Unit,
                -Group) |>
  dplyr::left_join(deployments |>
                     dplyr::select(Station.name,
                                   Receiver,
                                   Start,
                                   Stop,
                                   Latitude,
                                   Longitude,
                                   ReceiverDepthM,
                                   Array,
                                   Section,
                                   ReceiverDepthBin),
                   by = join_by(Receiver,
                                between(Timestamp, Start, Stop))) |>
  dplyr::select(-Start,
                -Stop) |>
  # join biometrics
  dplyr::left_join(biometrics |>
                     dplyr::select(Release.date,
                                   Group:FL_cm,
                                   Mature,
                                   acoustic_transmitter_SN,
                                   CodeSpaceSignal,
                                   Notes),
                   by = "CodeSpaceSignal") |>
  dplyr::filter(Group == "Bull")
rm(list = c("deployments", "biometrics")) # cleanup
# rm(biometrics)



#  Create csv of all receivers ####
detections |>
  group_by(Station.name) |>
  summarise_all(first) |>
  select(Station.name, Latitude, Longitude) |>
  # bind_rows(tibble(Station.name = c("Jupiter", "Deep Ledge"), # then manually added Jupiter & Deep Ledge: release sites, no hydrophone
  #                  Latitude = c(26.89924, 26.8925),
  #                  Longitude = c(-79.98325, -79.98666667))) |>
  arrange(desc(Latitude)) |>
  mutate(Owner = as.character("FACT")) |>
  mutate(Owner = ifelse(Station.name %in% c("PrncsA",
                                            "StJaqs", # "Governors wrecks", same, release site, no hydrophone
                                            "RublPl",
                                            "CCBrge",
                                            # "Deep ledge",
                                            "MG111"
                                            # "Jupiter"
  ),
  "FIU",
  Owner)) |> # replace NA with FALSE (or anything) across a list of columns, in a mutate call.
  write_csv(file = file.path(loadloc, "Maps", "AllSitesLatLon.csv"))

# Full original names in spatial.csv
# c("Princess Anne",
#   "St Jaques",
#   "Rubble Pile",
#   "Cross current barge",
#   "Deep ledge", # release site, no hydrophone
#   "MG111",
#   "Governors wrecks", # release site, no hydrophone
#   "Jupiter") # release site, no hydrophone




detectionsfilt <- detections |> filter(Station.name %in% c("PrncsA", # only FIU receivers
                                                           "StJaqs",
                                                           "RublPl",
                                                           "CCBrge",
                                                           "MG111")) |>
  mutate(Station.name = case_match(Station.name,
                                   "PrncsA" ~ "Princess Anne",
                                   "StJaqs" ~ "St Jaques",
                                   "RublPl" ~ "Rubble Pile",
                                   "CCBrge" ~ "Cross current barge",
                                   .default = Station.name))

# Abacus plot ####
ggplot(data = detections |>
         filter(Station.name %in% c("PrncsA", # only FIU receivers
                                    "StJaqs",
                                    "RublPl",
                                    "CCBrge",
                                    "MG111")) |>
         mutate(
           Station.name = case_match(Station.name,
                                     "PrncsA" ~ "Princess Anne",
                                     "StJaqs" ~ "St Jaques",
                                     "RublPl" ~ "Rubble Pile",
                                     "CCBrge" ~ "Cross current barge",
                                     .default = Station.name),
           Date = as.Date(Timestamp),
           Shark = factor(Signal,
                          # Can't pass original detections object as it's changed: use dplyr::pick(tidyselect::everything()). pick replaces depreciated cur_data
                          # https://stackoverflow.com/questions/79431077/reference-the-current-state-of-a-piped-object-in-r-pipe-subfunction
                          levels = pick(everything()) |> # colour-ordered by shark size (default is shark ID ("Signal")): Shark factor order by size
                            arrange(TSL_cm) |>
                            pull(Signal) |>
                            unique()),
           Station = factor(Station.name, # Station factor order by latitude
                            levels = pick(everything()) |> # colour-ordered by shark size (default is shark ID ("Signal")): Shark factor order by size
                              arrange(Latitude) |>
                              pull(Station.name) |>
                              unique()
           )
         ) |>
         droplevels() # drop unused factor levels
) +
  # border & shape
  geom_point(
    mapping = aes(
      x = Date,
      y = Station,
      fill = Shark, # definitely edits fill but all legend items are black (pch 21:24)
      # colour = factor(locationTagged, levels = c("New Brunswick, Canada", # definitely edits border colour but legend icons are filled  (pch 21:24)
      #                                            "Cape Cod, USA",
      #                                            "South Carolina, USA")),
      shape = Sex # @ 2025-02-10 only females
    ),
    size = 4,
    stroke = 0.7, # point border width
  ) +
  # scale_colour_manual(values = c("black", "blue", "red")) + # for border colour
  # geom_point(data = deployments |> # small x's for when receivers are first deployed
  #              dplyr::rename(Station = Station.name), # can't use cos Start falls back to first detection as we don't have actual deployment dates for other teams' gear
  #            mapping = aes(x = as.Date(Start),
  #                          y = Station),
  #            shape = 4) +
  scale_shape_manual(values = c(21:24)) + # have to be the shapes which have fill and colour # Only 1 sex
  guides(fill = guide_legend(override.aes = list(shape = 21))) + # https://stackoverflow.com/questions/77883100/ggplot-buggy-fill-and-colour-legends-for-shapes-pch-2125
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               date_minor_breaks = "1 month") +
  theme_classic() %+replace% theme(
    axis.text = element_text(size = rel(1.3)),
    axis.text.x = element_text(angle = 90), # , vjust = 1, hjust = 1
    axis.title = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1.5)),
    plot.background = element_rect(fill = "white", colour = "grey50"), # white background
    strip.text.x = element_text(size = rel(2)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank(),
    legend.spacing.x = unit(0, "cm"), # compress spacing between legend items, this is min
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid = element_line(colour = "grey90"),
    legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(lubridate::today(), "_Abacus.png"),
       plot = last_plot(), device = "png", path = file.path(loadloc), scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 8.5, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
       height = 3.2, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", dpi = 600, limitsize = TRUE)
# Then move from /Data to /Outputs













# Detection map all ####
# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(detections$Longitude),
                min(detections$Latitude),
                max(detections$Longitude),
                max(detections$Latitude))
googlemap = TRUE
if (googlemap) { # grow bounds extents if requested
  expandfactor <- 2 #1.6,3,6, 1.3 - 1.5 same zoom as 1. 1.6 is a big leap up in zoom (out). 1.9 (& maybe 1.7 or 1.8 is another step out, done manually for #2, 200368, because some points were out of bounds.)
  # Still need to improve this part
  xmid <- mean(myLocation[c(1,3)])
  ymid <- mean(myLocation[c(2,4)])
  xmax <- ((myLocation[3] - xmid) * expandfactor) + xmid #updated for sf/st
  xmin <- xmid - ((xmid - myLocation[1]) * expandfactor)
  ymax <- ((myLocation[4] - ymid) * expandfactor) + ymid
  ymin <- ymid - ((ymid - myLocation[2]) * expandfactor)
  myLocation <- c(xmin, ymin, xmax, ymax)
}
# ggmap::register_google("see STB/Code")
myMap <- ggmap::get_map(location = myLocation,
                        source = "google",
                        maptype = "satellite",
                        crop = FALSE)

autoheight <- (6 / (attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]])) * (attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]]) * 1.2

allpings <- detections |>
  group_by(Station.name) |>
  summarise(Longitude = mean(Longitude),
            Latitude = mean(Latitude),
            nHits = n()) |>
  select(Longitude, Latitude, everything())

ggmap::ggmap(myMap) +
  geom_point(data = allpings,
             aes(x = Longitude,
                 y = Latitude,
                 size = nHits), # points per hit, coloured by month   fill = monthcollected
             shape = 1, # 19 = filled circle, 1 = empty
             colour = "black") +
  scale_radius(range = c(0.01, 30)) +
  geom_label(data = detections |> # receivers
               group_by(Station.name) |>
               summarise(Latitude = mean(Latitude),
                         Longitude = mean(Longitude)),
             aes(x = Longitude,
                 y = Latitude,
                 label = Station.name), # points per hit, coloured by month   fill = monthcollected
             nudge_x = 0.7,
             nudge_y = -0.13,
             size = 2.2) +
  labs(x = "Longitude", y = "Latitude", caption = paste0("FIU BullShark, ", lubridate::today())) +
  ggtitle(paste0("Bull shark acoustic detections off SE Florida, USA")) +
  theme(legend.position.inside = c(0.1, 0.16), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.spacing.y = unit(0.1, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_rect(fill = "white", colour = NA), # element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(today(), "_Bullshark-Map.png"),
       plot = last_plot(), device = "png", path = loadloc, scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 5, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
       height = 5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", dpi = 600, limitsize = TRUE)




# Detection map Ours ####
detectionsours <- detections |>
  mutate(Station.name = case_match(Station.name,
                                   "PrncsA" ~ "Princess Anne",
                                   "StJaqs" ~ "St Jaques",
                                   "RublPl" ~ "Rubble Pile",
                                   "CCBrge" ~ "Cross current barge",
                                   .default = Station.name)) |>
  filter(Station.name %in% c("Princess Anne",
                             "St Jaques",
                             "Rubble Pile",
                             "Cross current barge",
                             "MG111"))

# bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
myLocation <- c(min(detectionsours$Longitude),
                min(detectionsours$Latitude),
                max(detectionsours$Longitude),
                max(detectionsours$Latitude))


# IMPROVE & COPY TO PLOTRASTER.R ####
if (googlemap) { # grow bounds extents if requested
  expandfactor <- NULL #1.6,3,6, 1.3 - 1.5 same zoom as 1. 1.6 is a big leap up in zoom (out). 1.9 (& maybe 1.7 or 1.8 is another step out, done manually for #2, 200368, because some points were out of bounds.)
  # Still need to improve this part
  expandx <- 8
  expandy <- 1.1
  xmid <- mean(myLocation[c(1,3)])
  ymid <- mean(myLocation[c(2,4)])
  ifelse(is.null(expandx), expandfactor, expandx)
  xmax <- ((myLocation[3] - xmid) * ifelse(is.null(expandx), expandfactor, expandx)) + xmid #updated for sf/st
  xmin <- xmid - ((xmid - myLocation[1]) * ifelse(is.null(expandx), expandfactor, expandx))
  ymax <- ((myLocation[4] - ymid) * ifelse(is.null(expandy), expandfactor, expandy)) + ymid
  ymin <- ymid - ((ymid - myLocation[2]) * ifelse(is.null(expandy), expandfactor, expandy))
  myLocation <- c(xmin, ymin, xmax, ymax)
}

# ggmap::register_google("see STB/Code")
# ggmap::register_stadiamaps(key = "see STB/Code", write = TRUE)
myMap <- ggmap::get_map(location = myLocation,
                        source = "stadia", # google
                        maptype = "stamen_terrain", # satellite
                        crop = FALSE)

autoheight <- (6 / (attr(myMap, "bb")[[4]] - attr(myMap, "bb")[[2]])) * (attr(myMap, "bb")[[3]] - attr(myMap, "bb")[[1]]) * 1.2

allpings <- detectionsours |>
  group_by(Station.name) |>
  summarise(Longitude = mean(Longitude),
            Latitude = mean(Latitude),
            nHits = n()) |>
  select(Longitude, Latitude, everything())

ggmap::ggmap(myMap) +
  geom_point(data = allpings,
             aes(x = Longitude,
                 y = Latitude,
                 size = nHits), # points per hit, coloured by month   fill = monthcollected
             shape = 1, # 19 = filled circle, 1 = empty
             colour = "black") +
  scale_radius(range = c(0.01, 30)) +
  ggrepel::geom_label_repel(data = allpings,
                            aes(x = Longitude,
                                y = Latitude,
                                label = paste0(Station.name, ": ", nHits)), # points per hit, coloured by month   fill = monthcollected
                            # nudge_x = 0.035,
                            # nudge_y = -0.13,
                            # position = position_jitter(width = 0.01, height = ifelse(allpings$Station.name == "Princess Anne", 0.02, 0)),
                            size = 2.2) +
  labs(x = "Longitude", y = "Latitude", caption = paste0("FIU BullShark, ", lubridate::today())) +
  ggtitle(paste0("Bull shark acoustic detections off SE Florida, USA")) +
  theme(
    legend.position = "none"
    # legend.position.inside = c(0.1, 0.16), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
    # legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
    # legend.spacing.y = unit(0.1, 'cm'), #compress spacing between legend items, this is min
    # legend.background = element_rect(fill = "white", colour = NA), # element_blank(),
    # panel.background = element_rect(fill = "white", colour = "grey50"), # white background
    # legend.key = element_blank()
  ) # removed whitespace buffer around legend boxes which is nice

ggsave(paste0(today(), "_Bullshark-Map-FL.png"),
       plot = last_plot(), device = "png", path = loadloc, scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
       width = 3.25, # 8 for Med # 7 normal # 3.8 miwingWhotspot, 7 wholearea 6 gsl 5 gom 5.5 centralAtl
       height = 5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
       units = "in", dpi = 600, limitsize = TRUE)

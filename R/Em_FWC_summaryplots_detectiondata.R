###### Summary plots for all regions ######

library(tidyverse)
library(data.table)
library(lubridate)
library(ggrepel)

setwd("E:/PEC Lab/Detection Data")

# Import plot functions
source(paste0("E:/PEC Lab/Detection Data/abacus and bubble plot functions.R"))

##### Read metadata ####
sharktags <- read_csv("tags/Shark_Tagging_Metadata.csv",
  col_types = cols(date_encountered = col_date(format = "%Y-%m-%d"))
) # %m/%d/%Y

# stations
stations <- read_csv("Receiver_Deployment_Log.csv") %>%
  filter(Retrieval_status == "success")

##### Read receiver (VR2 series) data ####
list <- list.files(paste0("E:/PEC Lab/Detection Data/Detections"), pattern = ".csv")
list
detections <- NULL
for (i in list[
  c(grep("FWC", list))
]) {
  print(i)
  temp <- read_csv(paste0("E:/PEC Lab/Detection Data/Detections/", i),
    col_types = cols(
      `Date and Time (UTC)` = col_datetime(format = ""),
      Receiver = col_character(),
      Transmitter = col_character(),
      `Transmitter Name` = col_character(),
      `Transmitter Serial` = col_character(),
      `Sensor Value` = col_character(),
      `Sensor Unit` = col_character(),
      `Station Name` = col_character(),
      Latitude = col_double(),
      Longitude = col_double()
    )
  ) %>%
    select(
      `Date and Time (UTC)`,
      Receiver,
      Transmitter,
      `Transmitter Serial`,
      `Sensor Value`,
      `Sensor Unit`,
      `Station Name`,
      Latitude,
      Longitude
    )
  detections <- rbind(detections, temp)
}
rm(temp, list, i)

detections <- detections %>%
  mutate(
    `Date and Time (UTC)` = `Date and Time (UTC)` %>% as.POSIXct(tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
    DateTimeET = `Date and Time (UTC)` %>% with_tz("America/New_York")
  )

min(detections$DateTimeET)
max(detections$DateTimeET)


##### Dates and Data you want to plot #####
# Start.date <- min(detections$DateTimeET)
End.date <- max(detections$DateTimeET)
Start.date <- as.POSIXct("2023-09-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
# End.date <- as.POSIXct("2024-04-04 00:00:00", format = "%Y-%m-%d %H:%M:%S")


detections <- filter(detections, DateTimeET >= Start.date & DateTimeET <= End.date)

##### Filter data and join metadata ####
# our sharks
shark <- filter(detections, Transmitter %in% sharktags$acoustic_transmitter) %>%
  left_join(sharktags, by = c("Transmitter" = "acoustic_transmitter")) %>%
  left_join(stations, by = c("Station Name" = "Station"))

# remove cols where all values are NA
shark <- shark[, colSums(is.na(shark)) < nrow(shark)]


##### White Shark tagging summary #####
unique(shark$`Station Name`)


# all sharks detected during selected interval
if (T) { # Change to T to include, F to exclude
  library(gridExtra)
  pdf(paste0("E:/PEC Lab/Detection Data/Detection Data Plots/shark-TagSummary-FWC-", Sys.Date(), ".pdf"),
    width = 8.5,
    height = 11
  )
  grid.table(
    shark %>%
      distinct(Transmitter) %>%
      left_join(sharktags, by = c("Transmitter" = "acoustic_transmitter")) %>%
      mutate(
        `Length (ft)` = (TSL_cm / 30.48) %>% round(1),
        Year = date_encountered %>% as.Date() %>% year()
      ) %>%
      select(
        species,
        Transmitter,
        date_encountered,
        TSL_cm,
        `Length (ft)`,
        sex,
        tag_location
      ) %>%
      rename(
        Species = species,
        `Encounter date` = date_encountered,
        `Length (cm)` = TSL_cm,
        Sex = sex,
        `Tag location` = tag_location
      ) %>%
      arrange(Species)
  )
  dev.off()
}


##### Plots - FWC Bull Shark Abacus and Bubble Plot #####
# nonjws detections
if (any(shark %>% distinct(Region) == "Palm Beach")) {
  abacus_plt <- abacus_plot(shark %>% filter(Region == "Palm Beach"),
    date_breaks = "1 month",
    type = 4
  )
  bubble_plt <- bubble_plot_map(shark %>% filter(Region == "Palm Beach"),
    # direction = "x",
    nudge_y = 0.01,
    nudge_x = 0.1,
    force = 10,
    lon_min = -80.1496,
    lon_max = -79.8,
    lat_min = 26.7223,
    lat_max = 26.99258
  )

  plt <- cowplot::plot_grid(
    bubble_plt + theme(plot.margin = unit(c(t = 0.2, r = 1, b = 0, l = 1), "cm")) +
      ggtitle("Palm Beach County Detections"),
    abacus_plt + theme(plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")),
    ncol = 1,
    rel_heights = c(.5, .5),
    axis = "l",
    align = "h"
  )
  ggsave(paste0("E:/PEC Lab/Detection Data/Detection Data Plots/WestPalm-", Sys.Date(), ".png"),
    plot = plt,
    height = 9,
    width = 8.5,
    units = "in"
  )
}

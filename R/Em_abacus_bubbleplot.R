# Abacus plot types
abacus_types <- function(f) {
  cat("Type 1: Shark ID (x) and stations (y)\nType 2: Shark ID (y)\nType 3: Transmitter (color) and station (y)\nType 4: NonJWS (color) and station (y)\nType 5: Plot raw detection data (columns: `Date Time (UTC)` and Transmitter)")
}
##### Define abacus plot function #####
abacus_plot <- function(data,
                        type = 1,
                        date_breaks = "1 month",
                        date_labels = "%Y %b %d") {
  if (type == 1) { # shark ID (color) and stations (y)
    plt <-
      data %>%
      ggplot() +
      geom_jitter(
        aes(DateTimeET,
            fct_reorder(`Station Name`, Latitude),
            color = shark
        ),
        height = 0.4
      ) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) + 0.5), colour = "black", linewidth = 0.25) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) - 0.5), colour = "black", linewidth = 0.25) +
      labs(x = "DateTimeET", y = "Station", color = "Shark ID") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(
        limits = c(Start.date, End.date),
        date_labels = date_labels, date_breaks = date_breaks,
        expand = expansion(mult = c(0, .01))
      ) +
      scale_y_discrete(expand = expansion(mult = 0, add = c(0.5, 0.5))) +
      scale_color_viridis_d()
  }
  if (type == 2) { # shark ID (y)
    plt <-
      data %>%
      ggplot() +
      geom_jitter(
        aes(as.POSIXct(DateTimeET),
            fct_reorder(shark, when_encountered),
            color = fct_reorder(shark, when_encountered)
        ),
        height = 0.4
      ) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(shark, when_encountered)) + 0.5), colour = "black", linewidth = 0.25) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(shark, when_encountered)) - 0.5), colour = "black", linewidth = 0.25) +
      labs(x = "DateTimeET", y = "Shark ID", color = "shark") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(
        limits = c(Start.date, End.date),
        date_labels = date_labels, date_breaks = date_breaks,
        expand = expansion(mult = c(0, .01))
      ) +
      scale_y_discrete(expand = expansion(mult = 0, add = c(0.5, 0.5))) +
      scale_color_viridis_d()
  }
  if (type == 3) { # transmitter (color) and station (y)
    plt <-
      data %>%
      ggplot() +
      geom_jitter(
        aes(DateTimeET,
            fct_reorder(`Station Name`, Latitude),
            color = Transmitter
        ),
        height = 0.4
      ) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) + 0.5), colour = "black", linewidth = 0.25) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) - 0.5), colour = "black", linewidth = 0.25) +
      labs(x = "DateTimeET", y = "Station", color = "Transmitter") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(
        limits = c(Start.date, End.date),
        date_labels = date_labels, date_breaks = date_breaks
      ) +
      scale_y_discrete(expand = expansion(mult = 0, add = c(0.5, 0.5))) +
      scale_color_viridis_d()
  }
  if (type == 4) { # nonJWS (color) and station (y)
    plt <-
      data %>%
      ggplot() +
      geom_jitter(
        aes(DateTimeET,
            fct_reorder(`Station Name`, Latitude),
            color = paste(Transmitter, common_name)
        ),
        height = 0.4
      ) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) + 0.5), colour = "black", linewidth = 0.25) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(`Station Name`, Latitude)) - 0.5), colour = "black", linewidth = 0.25) +
      labs(x = "DateTimeET", y = "Station", color = "Transmitter and species") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(
        limits = c(Start.date, End.date),
        date_labels = "%Y %b %d", date_breaks = date_breaks
      ) +
      scale_y_discrete(expand = expansion(mult = 0, add = c(0.5, 0.5))) +
      scale_color_viridis_d()
    plt
  }
  if (type == 5) { # plot raw detection data (columns: `Date Time (UTC)` and Transmitter)
    plt <-
      data %>%
      mutate(`Date and Time (UTC)` = `Date and Time (UTC)` %>% as_datetime()) %>%
      ggplot() +
      geom_jitter(
        aes(
          `Date and Time (UTC)`,
          Transmitter
        ),
        height = 0.4
      ) +
      labs(x = "DateTimeUTC", y = "Transmitter") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(date_labels = "%Y %b %d", date_breaks = date_breaks) +
      scale_color_viridis_d()
  }
  print(plt)
  if (type == 6) { # non jws ID (color) and stations (y)
    plt <-
      data %>%
      ggplot() +
      geom_jitter(
        aes(DateTimeET,
            fct_reorder(Transmitter, date_deployed),
            color = species
        ),
        height = 0.4
      ) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(Transmitter, date_deployed)) + 0.5), colour = "black", linewidth = 0.25) +
      geom_hline(aes(yintercept = as.numeric(fct_reorder(Transmitter, date_deployed)) - 0.5), colour = "black", linewidth = 0.25) +
      labs(x = "DateTimeET", y = "Transmitter", color = "Species") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0.2, r = 1, b = 1, l = 1), "cm")
      ) +
      scale_x_datetime(
        limits = c(Start.date, End.date),
        date_labels = date_labels, date_breaks = date_breaks,
        expand = expansion(mult = c(0, .01))
      ) +
      scale_y_discrete(expand = expansion(mult = 0, add = c(0.5, 0.5))) +
      scale_color_viridis_d()
  }
  print(plt)
}

# Define map function
bubble_plot_map <- function(data,
                            map_margin = 0.01,
                            lon_push = 0,
                            lat_push = 0,
                            nudge_x = 0,
                            nudge_y = 0,
                            vjust = 0.5,
                            hjust = 0.5,
                            xlim = c(NA, NA),
                            ylim = c(NA, NA),
                            direction = "both",
                            force = 1,
                            range = c(1, 12),
                            box.padding = 0.25,
                            lon_min = min(data$Lng_receiver) - map_margin + lon_push, # Lat & Lon_receiver not genreated til later
                            lon_max = max(data$Lng_receiver) + map_margin + lon_push,
                            lat_min = min(data$Lat_receiver) - map_margin + lat_push,
                            lat_max = max(data$Lat_receiver) + map_margin + lat_push,
                            limits = c(1, max(data$Detections)), # not genreated til later
                            title = NULL,
                            trans = "identity",
                            label = TRUE,
                            mybreaks = "auto") {
  require(ggplot2)
  require(ggrepel)
  require(sf)
  # Get map polygons
  # map_dat <- raster::getData("GADM", country = "USA", level = 1)
  map_dat <- geodata::gadm(country = "USA", level = 1, path = file.path(loadloc, "Data", "Maps", "geodata_downloads"))
  # map_dat <- subset(map_dat, NAME_1 == "Florida")

  # Summarize data for bubbles
  data <- data %>%
    filter(DateTimeET >= Start.date & DateTimeET <= End.date) %>%
    group_by(`Station Name`) %>%
    summarize(
      Detections = n(),
      Lat_receiver = mean(Latitude),
      Lng_receiver = mean(Longitude)
    ) %>%
    arrange(Detections)

  if (mybreaks == "auto") {
    mybreaks <- seq.int(from = 1, to = max(data$Detections), length.out = 5) %>% round(0)
  }

  # Bubble Map with labels
  if (label == TRUE) {
    map <-
      data %>%
      ggplot() +
      # geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      # geom_sf(data = sf::st_as_sf(map_dat),
      tidyterra::geom_spatvector(data = map_dat,
                                 aes(
                                   # x = long,
                                   # y = lat,
                                   # group = group
                                 ), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = Detections, color = Detections), alpha = 0.5) +
      geom_label_repel(aes(x = Lng_receiver, y = Lat_receiver, label = `Station Name`),
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       vjust = vjust,
                       hjust = hjust,
                       direction = direction,
                       xlim = xlim,
                       ylim = ylim,
                       force = force,
                       box.padding = box.padding
      ) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(color = guide_legend()) +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }

  # Bubble Map without labels
  if (label == FALSE) {
    map <-
      data %>%
      ggplot() +
      geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = Detections, color = Detections), alpha = 0.5) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(color = guide_legend()) +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }
  print(data, n = 100)
  print(map)
}

bubble_plot_map_unique <- function(data,
                                   map_margin = 0.01,
                                   lon_push = 0,
                                   lat_push = 0,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   xlim = c(NA, NA),
                                   ylim = c(NA, NA),
                                   direction = "both",
                                   force = 1,
                                   range = c(1, 12),
                                   box.padding = 0.25,
                                   lon_min = min(data$Lng_receiver) - map_margin + lon_push,
                                   lon_max = max(data$Lng_receiver) + map_margin + lon_push,
                                   lat_min = min(data$Lat) - map_margin + lat_push,
                                   lat_max = max(data$Lat) + map_margin + lat_push,
                                   limits = c(1, max(data$n_sharks)),
                                   title = NULL,
                                   trans = "identity",
                                   label = FALSE,
                                   mybreaks = "auto") {
  require(ggplot2)
  require(ggrepel)
  require(sf)
  # Get map polygons
  map_dat <- raster::getData("GADM", country = "USA", level = 1)
  map_dat <- subset(map_dat, NAME_1 == "Florida")

  # Summarize data for bubbles
  data <-
    data %>%
    filter(DateTimeET >= Start.date & DateTimeET <= End.date) %>%
    group_by(Region) %>%
    summarize(
      n_sharks = n_distinct(shark),
      Lat_receiver = mean(Latitude, na.rm = T),
      Lng_receiver = mean(Longitude, na.rm = T)
    ) %>%
    arrange(n_sharks)

  if (mybreaks == "auto") {
    mybreaks <- seq.int(from = 1, to = max(data$n_sharks), length.out = 5) %>% round(0)
  }

  # Bubble Map with labels
  if (label == TRUE) {
    map <-
      data %>%
      ggplot() +
      geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_polygon(data = map_dat_MX, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = n_sharks, color = n_sharks), alpha = 0.5) +
      geom_label_repel(aes(x = Lng_receiver, y = Lat_receiver, label = Region),
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       direction = direction,
                       xlim = xlim,
                       ylim = ylim,
                       force = force,
                       box.padding = box.padding
      ) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(
        color = guide_legend("Unique sharks\ndetected"),
        size = guide_legend("Unique sharks\ndetected")
      ) +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }

  # Bubble Map without labels
  if (label == FALSE) {
    map <-
      data %>%
      ggplot() +
      geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_polygon(data = map_dat_MX, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = n_sharks, color = n_sharks), alpha = 0.5) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(
        color = guide_legend("Unique sharks\ndetected"),
        size = guide_legend("Unique sharks\ndetected")
      ) +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }
  print(data, n = 100)
  print(map)
}

# Define map function
bubble_plot_map_regions <- function(data,
                                    map_margin = 0.01,
                                    lon_push = 0,
                                    lat_push = 0,
                                    nudge_x = 0,
                                    nudge_y = 0,
                                    xlim = c(NA, NA),
                                    ylim = c(NA, NA),
                                    direction = "both",
                                    force = 1,
                                    range = c(1, 12),
                                    box.padding = 0.25,
                                    lon_min = min(data$Lng_receiver) - map_margin + lon_push,
                                    lon_max = max(data$Lng_receiver) + map_margin + lon_push,
                                    lat_min = min(data$Lat) - map_margin + lat_push,
                                    lat_max = max(data$Lat) + map_margin + lat_push,
                                    limits = c(1, max(data$Detections)),
                                    title = NULL,
                                    trans = "identity",
                                    label = TRUE,
                                    mybreaks = "auto") {
  require(ggplot2)
  require(ggrepel)
  require(sf)
  # Get map polygons
  map_dat <- raster::getData("GADM", country = "USA", level = 1)
  map_dat <- subset(map_dat, NAME_1 == "Florida")

  # Summarize data for bubbles
  data <-
    data %>%
    filter(DateTimeET >= Start.date & DateTimeET <= End.date) %>%
    group_by(Region) %>%
    summarize(
      Detections = n() / n_distinct(`Station Name`),
      Lat_receiver = mean(Latitude, na.rm = T),
      Lng_receiver = mean(Longitude, na.rm = T)
    ) %>%
    arrange(Detections)

  if (mybreaks == "auto") {
    mybreaks <- seq.int(from = 1, to = max(data$Detections), length.out = 5) %>% round(0)
  }

  # Bubble Map with labels
  if (label == TRUE) {
    map <-
      data %>%
      ggplot() +
      geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_polygon(data = map_dat_MX, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = Detections, color = Detections), alpha = 0.5) +
      geom_label_repel(aes(x = Lng_receiver, y = Lat_receiver, label = Region),
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       direction = direction,
                       xlim = xlim,
                       ylim = ylim,
                       force = force,
                       box.padding = box.padding
      ) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(color = guide_legend()) +
      labs(x = "Longitude", y = "Latitude", name = "Detections/\nNo. of receivers per region") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }

  # Bubble Map without labels
  if (label == FALSE) {
    map <-
      data %>%
      ggplot() +
      geom_polygon(data = map_dat, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_polygon(data = map_dat_MX, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgray", alpha = 0.3) +
      geom_point(aes(x = Lng_receiver, y = Lat_receiver, size = Detections, color = Detections), alpha = 0.5) +
      scale_color_viridis_c(trans = trans, breaks = mybreaks, limits = limits) +
      scale_size_continuous(trans = trans, range = range, breaks = mybreaks, limits = limits) +
      coord_map() +
      theme_bw() +
      coord_sf(
        xlim = c(lon_min, lon_max),
        ylim = c(lat_min, lat_max),
        expand = F
      ) +
      guides(
        color = guide_legend("Detections/\nNo. of receivers\nper region"),
        size = guide_legend("Detections/\nNo. of receivers\nper region")
      ) +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(t = 0, r = 1, b = 0, l = 1), "cm")
      ) +
      ggtitle(title)
  }
  print(map)
}

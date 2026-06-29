# pages/biodiversity/module/biomap.R
library(shiny)
library(leaflet)
library(lubridate)
library(digest)

ICON_SIZE <- 36
nz <- function(x) ifelse(!is.na(x) & nzchar(x), x, "")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
guess_levels_cols <- function(df) {
  cand <- c("kingdom", "taxa", "phylum", "class", "order", "family", "genus", "species")
  cand[cand %in% names(df)]
}
logi <- function(fmt, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[biomap][%s] %s\n", ts, sprintf(fmt, ...)))
}

# Stable UID: same row (same species/common name/original lat/lon) <U+2192> same UID
.make_uid <- function(species, common, lat, lon) {
  key <- paste0(
    nz(species), "|", nz(common), "|",
    sprintf("%.6f", as.numeric(lat)), "|", sprintf("%.6f", as.numeric(lon))
  )
  digest::digest(key, algo = "xxhash64")
}

biomap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # <U+2605> Minimum map height
    tags$style(HTML(sprintf("#%s { min-height: 420px; }", ns("map")))),
    leafletOutput(ns("map"), height = "100%", width = "100%"),

    # Tree <U+2192> map: receive tree click and convert to input (trigger highlight & popup)
    tags$script(htmltools::HTML(sprintf("
      Shiny.addCustomMessageHandler('%s', function(msg){
        if (msg && msg.name){
          Shiny.setInputValue('%s', msg.name, {priority:'event'});
        }
      });
    ", ns("focus_from_tree"), ns("rtree_click_from_js"))))
  )
}

biomap_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    # <U+2014><U+2014> Fallback functions if not provided externally <U+2014><U+2014> #
    if (!exists("bio_build_labels", mode = "function", inherits = TRUE)) {
      bio_build_labels <- function(display_name, taxa, kingdom, location, lat, lon) {
        dn <- display_name %||% rep("Unknown", length(lat))
        loc <- location %||% rep("", length(lat))
        sprintf(
          "<b>%s</b><br/><span style='opacity:.8'>%s</span>",
          htmltools::htmlEscape(dn), htmltools::htmlEscape(loc)
        )
      }
    }
    if (!exists("bio_build_popups", mode = "function", inherits = TRUE)) {
      bio_build_popups <- function(display_name, species, taxa, kingdom,
                                   phylum, class_, order_, family, genus,
                                   sighting, datares, notes, location, lat, lon, commonname) {
        dn <- display_name %||% rep("Unknown", length(lat))
        sp <- species %||% rep("", length(lat))
        tx <- taxa %||% rep("", length(lat))
        kg <- kingdom %||% rep("", length(lat))
        lc <- location %||% rep("", length(lat))
        st <- sighting %||% rep("", length(lat))
        out <- vector("list", length(dn))
        for (i in seq_along(dn)) {
          out[[i]] <- sprintf(
            "<div class='bio-popup'>
               <div style='font-weight:600;font-size:14px;margin-bottom:4px'>%s</div>
               <div style='font-size:12px;line-height:1.4'>
                 <div><b>Species:</b> %s</div>
                 <div><b>Taxa:</b> %s</div>
                 <div><b>Kingdom:</b> %s</div>
                 <div><b>Location:</b> %s</div>
                 <div><b>Sighting:</b> %s</div>
               </div></div>",
            htmltools::htmlEscape(dn[i]),
            htmltools::htmlEscape(sp[i] %||% ""),
            htmltools::htmlEscape(tx[i] %||% ""),
            htmltools::htmlEscape(kg[i] %||% ""),
            htmltools::htmlEscape(lc[i] %||% ""),
            htmltools::htmlEscape(st[i] %||% "")
          )
        }
        out
      }
    }

    # <U+2014><U+2014> Large jitter version overlap resolution with random direction <U+2014><U+2014> #
    offset_overlaps <- function(lon, lat, tol_m = 15, step_m = 10, ring_slots = 8) {
      n <- length(lon)
      if (n <= 0) {
        return(list(lon = lon, lat = lat, moved = rep(FALSE, n)))
      }
      mean_lat <- mean(lat, na.rm = TRUE)
      cosphi <- cos(mean_lat * pi / 180)
      dlat_deg_per_m <- 1 / 111320
      dlon_deg_per_m <- 1 / (111320 * max(cosphi, 1e-6))
      dlat_grid <- tol_m * dlat_deg_per_m
      dlon_grid <- tol_m * dlon_deg_per_m
      key_x <- floor(lon / dlon_grid + 0.5)
      key_y <- floor(lat / dlat_grid + 0.5)
      key <- paste0(key_x, "_", key_y)
      idx_by_key <- split(seq_len(n), key)
      new_lon <- lon
      new_lat <- lat
      moved <- rep(FALSE, n)
      base_m <- max(25, 4 * step_m)
      spread_m <- max(100, 2.0 * step_m)
      for (ids in idx_by_key) {
        k <- length(ids)
        if (k <= 1) next
        for (j in seq_len(k)) {
          id <- ids[j]
          if (j == 1) next
          frac <- sqrt((j - 1) / max(1, k - 1))
          r_m <- base_m + runif(1, 0, spread_m) * frac
          angle <- runif(1, 0, 2 * pi)
          new_lon[id] <- lon[id] + (r_m * cos(angle)) * dlon_deg_per_m
          new_lat[id] <- lat[id] + (r_m * sin(angle)) * dlat_deg_per_m
          moved[id] <- TRUE
        }
      }
      list(lon = new_lon, lat = new_lat, moved = moved)
    }

    # <U+2014><U+2014> Highlight (yellow stroke) icon (fallback to circle for invalid/NA shapes) <U+2014><U+2014> #
    svg_halo_uri <- function(shape, size = ICON_SIZE,
                             stroke = "#ffd700", stroke_w = 4.5, opacity = 0.95) {
      shape2 <- if (is.na(shape) || !(shape %in% c("circle", "diamond"))) "circle" else shape
      st <- gsub("#", "%23", stroke)
      if (identical(shape2, "circle")) {
        svg <- sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
             <circle cx="12" cy="12" r="10" fill="none" stroke="%s" stroke-opacity="%.2f"
                     stroke-width="%.2f" stroke-linejoin="round"/></svg>',
          size, size, st, opacity, stroke_w
        )
      } else {
        svg <- sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
             <path d="M12 2 L22 12 L12 22 L2 12 Z" fill="none" stroke="%s" stroke-opacity="%.2f"
                   stroke-width="%.2f" stroke-linejoin="round"/></svg>',
          size, size, st, opacity, stroke_w
        )
      }
      paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
    }
    make_halo_icon <- function(shape) {
      icons(
        iconUrl = svg_halo_uri(shape = shape),
        iconWidth = ICON_SIZE, iconHeight = ICON_SIZE,
        iconAnchorX = ICON_SIZE / 2, iconAnchorY = ICON_SIZE / 2
      )
    }

    points_state <- reactiveVal(NULL)
    autofit_done <- reactiveVal(FALSE)

    # <U+2014><U+2014> Map rendering <U+2014><U+2014> #
    output$map <- renderLeaflet({
      leaflet() |>
        addMapPane("markers", zIndex = 420) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = 144.9631, lat = -37.8136, zoom = 13) |>
        addLayersControl(
          position = "bottomright",
          overlayGroups = c("Species"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # <U+2014><U+2014> Species markers <U+2014><U+2014> #
    observe({
      df <- data_reactive()
      if (is.null(df) || nrow(df) <= 0) {
        points_state(NULL)
        leafletProxy(session$ns("map"), session) |>
          clearGroup("Species") |>
          clearGroup("highlight")
        return()
      }
      getc <- function(nm, default = NA_character_) if (nm %in% names(df)) as.character(df[[nm]]) else rep(default, nrow(df))
      getn <- function(nm, default = NA_real_) if (nm %in% names(df)) as.numeric(df[[nm]]) else rep(default, nrow(df))

      lat_all <- if ("lat" %in% names(df)) getn("lat") else if ("latitude" %in% names(df)) getn("latitude") else rep(NA_real_, nrow(df))
      lon_all <- if ("lon" %in% names(df)) getn("lon") else if ("longitude" %in% names(df)) getn("longitude") else rep(NA_real_, nrow(df))
      keep <- is.finite(lat_all) & is.finite(lon_all)
      if (!any(keep)) {
        points_state(NULL)
        leafletProxy(session$ns("map"), session) |>
          clearGroup("Species") |>
          clearGroup("highlight")
        return()
      }

      # Subset
      lat <- lat_all[keep]
      lon <- lon_all[keep]
      display_name <- if ("display_name" %in% names(df)) {
        getc("display_name")[keep]
      } else {
        cn <- getc("commonname")[keep]
        sp <- getc("species")[keep]
        tx <- getc("taxa")[keep]
        ifelse(!is.na(cn) & nzchar(cn), cn,
          ifelse(!is.na(sp) & nzchar(sp), sp,
            ifelse(!is.na(tx) & nzchar(tx), tx, "Unknown")
          )
        )
      }
      taxa <- getc("taxa")[keep]
      kingdom <- getc("kingdom")[keep]
      phylum <- getc("phylum")[keep]
      class_ <- getc("class")[keep]
      order_ <- getc("order")[keep]
      family <- getc("family")[keep]
      genus <- getc("genus")[keep]
      species <- getc("species")[keep]
      datares <- getc("dataresourcename")[keep]
      notes <- getc("identificationnotes")[keep]
      location <- getc("location")[keep]
      sighting <- getc("sightingdate")[keep]
      common <- nz(getc("commonname")[keep])

      # Month
      parse_month <- function(x) {
        suppressWarnings({
          d <- ymd(x, quiet = TRUE)
          if (all(is.na(d))) d <- dmy(x, quiet = TRUE)
          if (all(is.na(d))) d <- mdy(x, quiet = TRUE)
        })
        m <- month(d)
        if (all(is.na(m)) && ("month" %in% names(df))) m <- as.integer(getn("month")[keep])
        if (all(is.na(m))) {
          mm <- suppressWarnings(as.integer(gsub(".*?(\\d{1,2}).*", "\\1", x)))
          m <- ifelse(!is.na(mm) & mm >= 1 & mm <= 12, mm, NA_integer_)
        }
        m
      }
      month_num <- parse_month(sighting)

      # Color and shape
      month_colors <- setNames(c(
        "#e41a1c", "#ff7f00", "#be4dea", "#55ca51", "#377eb8", "#984ea3",
        "#a65628", "#f781bf", "#1b9e77", "#7570b3", "#66a61e", "#e6ab02"
      ), as.character(1:12))
      fill_col <- month_colors[as.character(month_num)]
      fill_col[is.na(fill_col)] <- "#9e9e9e"

      k_lower <- tolower(kingdom)
      shape_vec <- ifelse(!is.na(k_lower) & k_lower %in% c("animalia", "fungi"), "diamond", "circle")

      # Generate UID (use "original" lat/lon, not jittered coordinates)
      uids <- mapply(.make_uid, species, common, lat, lon, USE.NAMES = FALSE)

      # Jittered coordinates for plotting
      adj <- offset_overlaps(lon, lat, tol_m = 15, step_m = 10, ring_slots = 8)
      lon_plot <- adj$lon
      lat_plot <- adj$lat

      # SVG icons (fallback to circle for invalid/NA shapes)
      svg_uri <- function(shape, fill, size = ICON_SIZE, stroke = "#ffffff", stroke_w = 0.8, fill_opacity = 0.72) {
        shape2 <- if (is.na(shape) || !(shape %in% c("circle", "diamond"))) "circle" else shape
        f <- gsub("#", "%23", fill)
        st <- gsub("#", "%23", stroke)
        if (identical(shape2, "circle")) {
          svg <- sprintf(
            '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
               <circle cx="12" cy="12" r="8" fill="%s" fill-opacity="%.2f" stroke="%s" stroke-width="%.2f"/></svg>',
            size, size, f, fill_opacity, st, stroke_w
          )
        } else {
          svg <- sprintf(
            '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
               <path d="M12 2 L22 12 L12 22 L2 12 Z" fill="%s" fill-opacity="%.2f" stroke="%s" stroke-width="%.2f"/></svg>',
            size, size, f, fill_opacity, st, stroke_w
          )
        }
        paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
      }
      icon_urls <- mapply(function(shape, fill) svg_uri(shape, fill, size = ICON_SIZE, stroke = "#ffffff", stroke_w = 0.8, fill_opacity = 0.72),
        shape_vec, fill_col,
        SIMPLIFY = TRUE
      )
      icons_vec <- icons(
        iconUrl = icon_urls,
        iconWidth = ICON_SIZE, iconHeight = ICON_SIZE,
        iconAnchorX = ICON_SIZE / 2, iconAnchorY = ICON_SIZE / 2,
        popupAnchorX = 0, popupAnchorY = -ICON_SIZE / 2
      )

      label_html <- bio_build_labels(display_name, taxa, kingdom, location, lat, lon)
      popup_html <- bio_build_popups(
        display_name, species, taxa, kingdom, phylum, class_, order_, family, genus,
        sighting, datares, notes, location, lat, lon, common
      )
      popup_html_chr <- as.character(popup_html)

      leafletProxy(session$ns("map"), session) |>
        clearGroup("Species") |>
        addMarkers(
          lng = lon_plot, lat = lat_plot,
          layerId = paste0("uid:", uids), # <U+2605> Unique layerId
          group = "Species",
          icon = icons_vec,
          label = lapply(label_html, htmltools::HTML),
          labelOptions = labelOptions(
            direction = "auto", sticky = TRUE, opacity = 1, textsize = "13px",
            style = list("pointer-events" = "none"), className = "bio-label"
          ),
          popup = lapply(popup_html, htmltools::HTML),
          popupOptions = popupOptions(maxWidth = 360, closeButton = TRUE, className = "bio-popup-outer")
        )

      # For click lookup
      levs <- guess_levels_cols(df)
      path_df <- data.frame(
        idx = seq_along(lon_plot),
        leaf_uid = uids,
        display_name = display_name,
        species = species,
        lat = lat_plot, lon = lon_plot,
        location = location,
        commonname = nz(common),
        month = as.integer(month_num),
        popup_html = popup_html_chr,
        orig_lat = lat, orig_lon = lon,
        shape = shape_vec,
        stringsAsFactors = FALSE
      )
      for (nm in levs) {
        val <- nz(getc(nm)[keep])
        val[!nzchar(val)] <- "Unknown"
        path_df[[nm]] <- val
      }
      points_state(path_df)

      # Auto-fit to point range only on first render
      if (!isTRUE(autofit_done())) {
        rng_lon <- range(lon_plot, na.rm = TRUE)
        rng_lat <- range(lat_plot, na.rm = TRUE)
        if (all(is.finite(rng_lon)) && all(is.finite(rng_lat)) &&
          diff(rng_lon) > 1e-6 && diff(rng_lat) > 1e-6) {
          leafletProxy(session$ns("map"), session) |>
            fitBounds(rng_lon[1], rng_lat[1], rng_lon[2], rng_lat[2])
          autofit_done(TRUE)
        }
      }
    })

    # <U+2014><U+2014> Map click: exact UID match + highlight + notify tree <U+2014><U+2014> #
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      id_raw <- click$id
      if (is.null(id_raw)) {
        return()
      }
      id <- as.character(id_raw)
      if (!length(id) || is.na(id) || !nzchar(id)) {
        return()
      }
      if (substr(id, 1, 8) == "__halo__") {
        return()
      }

      uid <- if (grepl("^uid:", id)) {
        sub("^uid:", "", id)
      } else {
        if (grepl(":", id, fixed = TRUE)) sub("^([^:]+):.*$", "\\1", id) else id
      }

      pts <- points_state()
      if (is.null(pts) || nrow(pts) <= 0) {
        return()
      }
      row <- pts[pts$leaf_uid == uid, , drop = FALSE]
      if (!nrow(row)) {
        return()
      }
      row <- row[1, , drop = FALSE]

      shape_safe <- if (is.na(row$shape[1]) || !(row$shape[1] %in% c("circle", "diamond"))) "circle" else row$shape[1]
      halo_icon <- make_halo_icon(shape_safe)

      leafletProxy(session$ns("map"), session) |>
        clearGroup("highlight") |>
        addMarkers(
          lng = row$lon, lat = row$lat, icon = halo_icon,
          layerId = paste0("__halo__", row$idx), group = "highlight",
          options = markerOptions(riseOnHover = TRUE)
        ) |>
        clearPopups() |>
        addPopups(
          lng = row$lon, lat = row$lat, popup = htmltools::HTML(row$popup_html),
          options = popupOptions(closeButton = TRUE, className = "bio-popup-outer", maxWidth = 360)
        )

      # Send to left tree (includes UID, prefer UID match)
      levs <- intersect(c("kingdom", "taxa", "phylum", "class", "order", "family", "genus", "species"), names(row))
      vals <- as.character(row[1, levs, drop = TRUE])
      vals[is.na(vals) | !nzchar(vals)] <- "Unknown"
      session$sendCustomMessage("bio_tree_highlight_path", list(
        path = c("Biodiversity", vals),
        name = row$display_name[1],
        uid = row$leaf_uid[1],
        month = if ("month" %in% names(row)) as.integer(row$month[1]) else NA_integer_,
        commonname = if ("commonname" %in% names(row)) row$commonname[1] else ""
      ))
    })

    # <U+2014><U+2014> Tree click <U+2192> map (name match) <U+2014><U+2014> #
    observeEvent(input$rtree_click_from_js, {
      nm <- input$rtree_click_from_js
      pts <- points_state()
      req(pts, nrow(pts) > 0, nzchar(nm))
      idx <- which(pts$display_name == nm)
      if (!length(idx) && "species" %in% names(pts)) idx <- which(pts$species == nm)
      if (!length(idx)) {
        return()
      }
      idx <- idx[1]
      lat <- as.numeric(pts$lat[idx])
      lon <- as.numeric(pts$lon[idx])
      shape_safe <- if (is.na(pts$shape[idx]) || !(pts$shape[idx] %in% c("circle", "diamond"))) "circle" else pts$shape[idx]
      halo_icon <- make_halo_icon(shape_safe)
      leafletProxy(session$ns("map"), session) |>
        setView(lng = lon, lat = lat, zoom = 15) |>
        clearGroup("highlight") |>
        addMarkers(
          lng = lon, lat = lat, icon = halo_icon,
          layerId = paste0("__halo__", pts$idx[idx]), group = "highlight",
          options = markerOptions(riseOnHover = TRUE)
        ) |>
        clearPopups() |>
        addPopups(
          lng = lon, lat = lat, popup = htmltools::HTML(pts$popup_html[idx]),
          options = popupOptions(closeButton = TRUE, className = "bio-popup-outer", maxWidth = 360)
        )
    })
  })
}

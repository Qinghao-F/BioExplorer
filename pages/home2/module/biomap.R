# pages/home2/module/biomap.R
library(shiny)
library(leaflet)
library(lubridate)

# Jitter coordinates, adjustable range
jitter_coords <- function(lat, lon, width = 0.0005, height = 0.0005) {
  lat_j <- lat + runif(length(lat), -height, height)
  lon_j <- lon + runif(length(lon), -width, width)
  list(lat = lat_j, lon = lon_j)
}


garden_sites <- reactive({
  path <- file.path(BIO_BASE, "dataset/garden_site.csv")
  if (!file.exists(path)) {
    return(NULL)
  }

  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- df[is.finite(df$Lat) & is.finite(df$Lon), ]
  df
})


ICON_SIZE <- 36
nz <- function(x) ifelse(!is.na(x) & nzchar(x), x, "")
guess_levels_cols <- function(df) {
  cand <- c("kingdom", "taxa", "phylum", "class", "order", "family", "genus", "species")
  cand[cand %in% names(df)]
}

home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "100%", width = "100%"),
    # --- New control buttons ---
    absolutePanel(
      top = 300, right = 20, width = 325,
      style = "
    z-index: 1000;
    background: rgba(255, 255, 255, 0.95);
    border-radius: 12px;
    padding: 15px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.25);
    overflow-y: auto;

    /* ✅ Adaptive viewport height to avoid overflow */
    max-height: calc(100vh - 300px);
    bottom: 20px;
  ",


      # ---- Toggle mode buttons ----
      tags$div(
        style = "margin-bottom: 15px;",
        radioButtons(
          ns("view_mode"),
          label = tags$b("Biodiversity Display Mode:"),
          choices = c("Biodiversity Points" = "points", "Heatmap" = "heatmap"),
          selected = "heatmap" # <- Change here
        )
      ),

      # ---- Bar chart area ----
      tags$div(
        style = "margin-bottom: 10px;",
        plotOutput(ns("site_bar"), height = "150px", width = "100%", click = ns("site_bar_click"))
      ),

      # ---- Dynamic content area ----
      tags$div(
        style = "border-top: 1px solid #ccc; padding-top: 10px;",
        htmlOutput(ns("site_title")),
        htmlOutput(ns("bird_sites_info")),
        uiOutput(ns("birds_button_ui"))
      )
    ),
    # ★ Receive "tree → map" focus message and write to this module's rtree_click_from_js
    tags$script(htmltools::HTML(sprintf("
      Shiny.addCustomMessageHandler('%s', function(msg){
        if (msg && msg.name){
          Shiny.setInputValue('%s', msg.name, {priority:'event'});
        }
      });
    ", ns("focus_from_tree"), ns("rtree_click_from_js"))))
  )
}

home_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    points_state <- reactiveVal(NULL)
    selected_type <- reactiveVal(NULL)


    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = 144.9631, lat = -37.8136, zoom = 13) |>
        addScaleBar(position = "bottomleft")
      # Floating image in bottom left corner
    })

    observe({
      df <- data_reactive()
      if (is.null(df) || !nrow(df)) {
        points_state(NULL)
        return()
      }

      getc <- function(nm, default = NA_character_) {
        if (nm %in% names(df)) as.character(df[[nm]]) else rep(default, nrow(df))
      }
      getn <- function(nm, default = NA_real_) {
        if (nm %in% names(df)) as.numeric(df[[nm]]) else rep(default, nrow(df))
      }

      # Latitude and longitude
      lat <- if ("lat" %in% names(df)) getn("lat") else if ("latitude" %in% names(df)) getn("latitude") else rep(NA_real_, nrow(df))
      lon <- if ("lon" %in% names(df)) getn("lon") else if ("longitude" %in% names(df)) getn("longitude") else rep(NA_real_, nrow(df))
      keep <- is.finite(lat) & is.finite(lon)
      if (!any(keep)) {
        points_state(NULL)
        return()
      }
      lat <- lat[keep]
      lon <- lon[keep]

      # Name (layerId)
      display_name <- if ("display_name" %in% names(df)) {
        getc("display_name")[keep]
      } else {
        cn <- getc("commonname")[keep]
        sp <- getc("species")[keep]
        tx <- getc("taxa")[keep]
        ifelse(!is.na(cn) & nzchar(cn), cn, ifelse(!is.na(sp) & nzchar(sp), sp, ifelse(!is.na(tx) & nzchar(tx), tx, "Unknown")))
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

      # Month (1..12)
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
          m <- ifelse(mm >= 1 & mm <= 12, mm, NA_integer_)
        }
        m
      }
      month_num <- parse_month(sighting)

      # 12-month color scheme (consistent with tree)
      month_colors <- setNames(
        c("#e41a1c", "#ff7f00", "#be4dea", "#55ca51", "#377eb8", "#984ea3", "#a65628", "#f781bf", "#1b9e77", "#7570b3", "#66a61e", "#e6ab02"),
        as.character(1:12)
      )
      fill_col <- rep("#9e9e9e", length(lat))
      fill_col[is.na(fill_col)] <- "#9e9e9e"

      # Shape (map: Animalia/Fungi=diamond, others=circle)
      k_lower <- tolower(kingdom)
      shape_vec <- ifelse(k_lower %in% c("animalia", "fungi"), "diamond", "circle")

      # SVG icons
      svg_uri <- function(shape, fill, size = ICON_SIZE, stroke = "#ffffff", stroke_w = 0.8, fill_opacity = 0.72) {
        f <- gsub("#", "%23", fill)
        st <- gsub("#", "%23", stroke)
        if (shape == "circle") {
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

      # label / popup (your existing constructor function)
      label_html <- bio_build_labels(display_name = display_name, taxa = taxa, kingdom = kingdom, location = location, lat = lat, lon = lon)
      popup_html <- bio_build_popups(
        display_name = display_name, species = species, taxa = taxa, kingdom = kingdom,
        phylum = phylum, class_ = class_, order_ = order_, family = family, genus = genus,
        sighting = sighting, datares = datares, notes = notes, location = location,
        lat = lat, lon = lon, commonname = common
      )
      popup_html_chr <- as.character(popup_html)

      leafletProxy("map", session) |>
        clearGroup("Biodiversity") |>
        clearGroup("Biodiversity Heatmap")

      if (input$view_mode == "points") {
        # ---- biodiversity markers ----
        coords_jitter <- jitter_coords(lat, lon)

        leafletProxy("map", session) |>
          addMarkers(
            lng = coords_jitter$lon, lat = coords_jitter$lat,
            layerId = display_name,
            icon = icons_vec,
            group = "Biodiversity",
            label = lapply(label_html, htmltools::HTML),
            labelOptions = labelOptions(
              direction = "auto", sticky = TRUE, opacity = 1, textsize = "13px",
              style = list("pointer-events" = "none"), className = "bio-label"
            ),
            popup = lapply(popup_html, htmltools::HTML),
            popupOptions = popupOptions(maxWidth = 300, closeButton = TRUE, className = "bio-popup-outer")
          )
      } else if (input$view_mode == "heatmap" && length(lat) > 0) {
        # ---- biodiversity heatmap ----
        leafletProxy("map", session) |>
          addHeatmap(
            lng = lon,
            lat = lat,
            intensity = rep(1, length(lat)),
            blur = 30,
            max = 0.05,
            radius = 15,
            gradient = colorRampPalette(c("#00FFFF", "#00CED1", "#32CD32", "#FFFFCC"))(10),
            group = "Biodiversity Heatmap"
          )
      }




      # ---- garden site markers ----
      df_garden <- garden_sites()
      if (!is.null(df_garden) && nrow(df_garden) > 0) {
        # Droplet SVG
        droplet_svg <- function(color = "#66CDAA", size = 36, stroke = "#004d00", fill_opacity = 0.4) {
          svg <- sprintf(
            '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
        <path d="M12 2 C12 2 2 12 12 22 C22 12 12 2 12 2 Z" fill="%s" stroke="%s" stroke-width="1"/></svg>',
            size, size, color, stroke
          )
          paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
        }

        garden_icons <- icons(
          iconUrl = rep(droplet_svg("#66CDAA", 36, "#004d00", fill_opacity = 0.4), nrow(df_garden)),
          iconWidth = 36, iconHeight = 36,
          iconAnchorX = 18, iconAnchorY = 36
        )

        leafletProxy("map", session) |>
          clearGroup("Garden Sites") |>
          addMarkers(
            lng = df_garden$Lon,
            lat = df_garden$Lat,
            icon = garden_icons,
            label = lapply(df_garden$Site, htmltools::HTML),
            popup = lapply(paste0("<b>", df_garden$Site, "</b><br>", df_garden$Address), htmltools::HTML),
            group = "Garden Sites"
          )
      }

      # ---- butterfly markers ----
      butterfly_path <- file.path(BIO_BASE, "dataset/butterfly.csv")
      if (file.exists(butterfly_path)) {
        df_butterfly <- read.csv(butterfly_path, stringsAsFactors = FALSE)
        keep <- is.finite(df_butterfly$Lat) & is.finite(df_butterfly$Lon)
        df_butterfly <- df_butterfly[keep, ]

        if (nrow(df_butterfly) > 0) {
          butterfly_icons <- icons(
            iconUrl = rep(droplet_svg("#cc99ff", 32, "#9900cc"), nrow(df_butterfly)),
            iconWidth = 32, iconHeight = 32,
            iconAnchorX = 16, iconAnchorY = 32
          )

          leafletProxy("map", session) |>
            clearGroup("Butterfly Sites") |>
            addMarkers(
              lng = df_butterfly$Lon,
              lat = df_butterfly$Lat,
              icon = butterfly_icons,
              label = lapply(df_butterfly$Site, htmltools::HTML),
              popup = lapply(paste0("<b>", df_butterfly$Site, "</b><br>", df_butterfly$Address), htmltools::HTML),
              group = "Butterfly Sites"
            )
        }
      }

      # ---- unique garden locations ----
      path_unique <- file.path(BIO_BASE, "dataset/unique_garden_locations.csv")
      if (file.exists(path_unique)) {
        df_unique <- read.csv(path_unique, stringsAsFactors = FALSE)
        keep <- is.finite(df_unique$lat) & is.finite(df_unique$lon)
        if (any(keep)) {
          df_unique <- df_unique[keep, ]
          unique_icons <- icons(
            iconUrl = rep(droplet_svg("#b22222", 32, "#800000"), nrow(df_unique)),
            iconWidth = 32, iconHeight = 32,
            iconAnchorX = 16, iconAnchorY = 32
          )

          leafletProxy("map", session) |>
            clearGroup("Unique Garden Sites") |>
            addMarkers(
              lng = df_unique$lon,
              lat = df_unique$lat,
              icon = unique_icons,
              label = lapply(df_unique$loc1_desc, htmltools::HTML),
              popup = lapply(df_unique$loc1_desc, htmltools::HTML),
              group = "Unique Garden Sites"
            )
        }
      }



      # layers control update



      # layers control




      # points_state: contains levels, commonname, month, popup_html, species (new) #
      levs <- guess_levels_cols(df)
      path_df <- data.frame(
        display_name = display_name,
        species = species, # ★ New: for tree click to match by species name
        lat = lat, lon = lon, location = location,
        commonname = common,
        month = as.integer(month_num),
        popup_html = popup_html_chr,
        stringsAsFactors = FALSE
      )
      for (nm in levs) {
        val <- nz(getc(nm)[keep])
        val[!nzchar(val)] <- "Unknown"
        path_df[[nm]] <- val
      }
      points_state(path_df)
    })

    # ---- Default bar chart hint (shown when bar chart is not clicked) ----
    output$site_title <- renderUI({
      HTML("<h4 style='text-align:center; margin:0; font-style:italic; color:#666;'>Click bars to see sites</h4>")
    })
    output$bird_sites_info <- renderUI({
      # Initially don't show specific list, only hint; can be changed to empty string "" or other note
      HTML("<div style='text-align:center; color:#666;'>&nbsp;</div>")
    })


    observeEvent(input$site_bar_click, {
      click_y <- input$site_bar_click$y
      counts <- data.frame(
        Type = c("Plant Sites", "Butterfly Sites", "Bird Sites"),
        Count = c(
          if (!is.null(garden_sites())) nrow(garden_sites()) else 0,
          if (file.exists(file.path(BIO_BASE, "dataset/butterfly.csv"))) nrow(read.csv(file.path(BIO_BASE, "dataset/butterfly.csv"))) else 0,
          if (file.exists(file.path(BIO_BASE, "dataset/unique_garden_locations.csv"))) nrow(read.csv(file.path(BIO_BASE, "dataset/unique_garden_locations.csv"))) else 0
        ),
        Color = c("#66CDAA", "#cc99ff", "#b22222"),
        stringsAsFactors = FALSE
      )

      # compute bar positions the same way as in renderPlot (horiz = TRUE)
      bar_positions <- barplot(
        counts$Count,
        names.arg = counts$Type,
        col = counts$Color,
        border = NA,
        horiz = TRUE,
        plot = FALSE
      )
      # find nearest bar index
      if (length(bar_positions) == 0 || is.null(click_y)) {
        return()
      }
      idx <- which.min(abs(bar_positions - click_y))
      if (length(idx) == 0 || is.na(idx)) {
        return()
      }

      # update title
      output$site_title <- renderUI({
        type <- counts$Type[idx]

        if (type == "Plant Sites") {
          HTML('<h4 style="text-align:center; margin:0;">Sites of Plants</h4>
          <div style="text-align:center; font-weight:normal; color:#666; font-size:small;">see more in plant page</div>')
        } else if (type == "Butterfly Sites") {
          HTML('<h4 style="text-align:center; margin:0;">Sites of Butterflies</h4>
          <div style="text-align:center; font-weight:normal; color:#666; font-size:small;">see more in butterfly page</div>')
        } else if (type == "Bird Sites") {
          HTML('<h4 style="text-align:center; margin:0;">Sites of Birds</h4>
          <div style="text-align:center; font-weight:normal; color:#666; font-size:small;">see more in bird page</div>')
        } else {
          HTML('<h4 style="text-align:center; margin:0;">Sites</h4>
          <div style="text-align:center; font-weight:normal; color:#666; font-size:small;"></div>')
        }
      })



      # update the textual list under the bar chart
      if (counts$Type[idx] == "Plant Sites") {
        df_garden <- garden_sites()
        if (!is.null(df_garden) && nrow(df_garden) > 0 && "Address" %in% names(df_garden)) {
          desc_text <- paste(df_garden$Site, collapse = "<br>")
          output$bird_sites_info <- renderUI({
            HTML(desc_text)
          })
        } else {
          output$bird_sites_info <- renderUI({
            HTML("")
          })
        }
      } else if (counts$Type[idx] == "Butterfly Sites") {
        butterfly_path <- file.path(BIO_BASE, "dataset/butterfly.csv")
        if (file.exists(butterfly_path)) {
          df_butterfly <- read.csv(butterfly_path, stringsAsFactors = FALSE)
          desc_text <- paste(df_butterfly$Site, collapse = "<br>")
          output$bird_sites_info <- renderUI({
            HTML(desc_text)
          })
        } else {
          output$bird_sites_info <- renderUI({
            HTML("")
          })
        }
      } else if (counts$Type[idx] == "Bird Sites") {
        path_unique <- file.path(BIO_BASE, "dataset/unique_garden_locations.csv")
        if (file.exists(path_unique)) {
          df_unique <- read.csv(path_unique, stringsAsFactors = FALSE)
          desc_text <- paste(df_unique$loc1_desc, collapse = "<br>")
          output$bird_sites_info <- renderUI({
            HTML(desc_text)
          })
        } else {
          output$bird_sites_info <- renderUI({
            HTML("")
          })
        }
      } else {
        output$bird_sites_info <- renderUI({
          HTML("")
        })
      }

      # -----------------------
      # Highlight the selected type and gray-out others (must be inside observeEvent)
      # -----------------------
      type_clicked <- counts$Type[idx]

      # helper droplet with fallback
      droplet_svg <- function(color = "#9e9e9e", size = 36, stroke = "#555555") {
        svg <- sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24">
        <path d="M12 2 C12 2 2 12 12 22 C22 12 12 2 12 2 Z"
        fill="%s" stroke="%s" stroke-width="1"/></svg>',
          size, size, color, stroke
        )
        paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
      }

      # clear old layers first
      leafletProxy("map", session) |>
        clearGroup("Garden Sites") |>
        clearGroup("Butterfly Sites") |>
        clearGroup("Unique Garden Sites")

      # Plant sites
      df_garden <- garden_sites()
      if (!is.null(df_garden) && nrow(df_garden) > 0) {
        color <- if (type_clicked == "Plant Sites") "#66CDAA" else "#cccccc"
        stroke <- if (type_clicked == "Plant Sites") "#004d00" else "#888888"
        icons_garden <- icons(
          iconUrl = rep(droplet_svg(color, 36, stroke), nrow(df_garden), fill_opacity = 0.4),
          iconWidth = 36, iconHeight = 36,
          iconAnchorX = 18, iconAnchorY = 36
        )
        leafletProxy("map", session) |>
          addMarkers(
            lng = df_garden$Lon,
            lat = df_garden$Lat,
            icon = icons_garden,
            label = lapply(df_garden$Site, htmltools::HTML),
            popup = lapply(paste0("<b>", df_garden$Site, "</b><br>", df_garden$Address), htmltools::HTML),
            group = "Garden Sites",
            options = markerOptions(zIndexOffset = if (type_clicked == "Plant Sites") 1000 else 0)
          )
      }

      # Butterfly sites
      butterfly_path <- file.path(BIO_BASE, "dataset/butterfly.csv")
      if (file.exists(butterfly_path)) {
        df_butterfly <- read.csv(butterfly_path, stringsAsFactors = FALSE)
        keep <- is.finite(df_butterfly$Lat) & is.finite(df_butterfly$Lon)
        df_butterfly <- df_butterfly[keep, ]
        if (nrow(df_butterfly) > 0) {
          color <- if (type_clicked == "Butterfly Sites") "#cc99ff" else "#cccccc"
          stroke <- if (type_clicked == "Butterfly Sites") "#9900cc" else "#888888"
          butterfly_icons <- icons(
            iconUrl = rep(droplet_svg(color, 32, stroke), nrow(df_butterfly), fill_opacity = 0.4),
            iconWidth = 32, iconHeight = 32,
            iconAnchorX = 16, iconAnchorY = 32
          )
          leafletProxy("map", session) |>
            addMarkers(
              lng = df_butterfly$Lon,
              lat = df_butterfly$Lat,
              icon = butterfly_icons,
              label = lapply(df_butterfly$Site, htmltools::HTML),
              popup = lapply(paste0("<b>", df_butterfly$Site, "</b><br>", df_butterfly$Address), htmltools::HTML),
              group = "Butterfly Sites",
              options = markerOptions(zIndexOffset = if (type_clicked == "Butterfly Sites") 1000 else 0)
            )
        }
      }

      # Bird / unique garden locations
      path_unique <- file.path(BIO_BASE, "dataset/unique_garden_locations.csv")
      if (file.exists(path_unique)) {
        df_unique <- read.csv(path_unique, stringsAsFactors = FALSE)
        keep <- is.finite(df_unique$lat) & is.finite(df_unique$lon)
        df_unique <- df_unique[keep, ]
        if (nrow(df_unique) > 0) {
          color <- if (type_clicked == "Bird Sites") "#b22222" else "#cccccc"
          stroke <- if (type_clicked == "Bird Sites") "#800000" else "#888888"
          unique_icons <- icons(
            iconUrl = rep(droplet_svg(color, 32, stroke), nrow(df_unique), fill_opacity = 0.4),
            iconWidth = 32, iconHeight = 32,
            iconAnchorX = 16, iconAnchorY = 32
          )
          leafletProxy("map", session) |>
            addMarkers(
              lng = df_unique$lon,
              lat = df_unique$lat,
              icon = unique_icons,
              label = lapply(df_unique$loc1_desc, htmltools::HTML),
              popup = lapply(df_unique$loc1_desc, htmltools::HTML),
              group = "Unique Garden Sites",
              options = markerOptions(zIndexOffset = if (type_clicked == "Bird Sites") 1000 else 0)
            )
        }
      }
    })






    # Map click: highlight + send path and leaf meta to tree (preserved)
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click$id)) {
        return()
      }

      leafletProxy("map", session) |>
        clearGroup("highlight") |>
        addCircleMarkers(
          lng = click$lng, lat = click$lat, radius = 10, color = "#004d40", fillColor = "#ffb300",
          fillOpacity = 0.9, weight = 2, group = "highlight",
          popup = paste0("<b>", click$id, "</b>"),
          options = markerOptions(zIndexOffset = 1000)
        )

      pts <- points_state()
      if (is.null(pts) || !nrow(pts)) {
        return()
      }
      row <- pts[pts$display_name == click$id, , drop = FALSE]
      if (!nrow(row)) {
        return()
      }

      levs <- intersect(c("kingdom", "taxa", "phylum", "class", "order", "family", "genus", "species"), names(row))
      vals <- as.character(row[1, levs, drop = TRUE])
      vals[is.na(vals) | !nzchar(vals)] <- "Unknown"
      path_vec <- c("Biodiversity", vals)

      session$sendCustomMessage("bio_tree_highlight_path", list(
        path = path_vec,
        name = click$id,
        month = if ("month" %in% names(row)) as.integer(row$month[1]) else NA_integer_,
        commonname = if ("commonname" %in% names(row)) row$commonname[1] else ""
      ))
    })

    output$site_bar <- renderPlot({
      # Read three types of data
      df_garden <- garden_sites()
      butterfly_path <- file.path(BIO_BASE, "dataset/butterfly.csv")
      unique_path <- file.path(BIO_BASE, "dataset/unique_garden_locations.csv")

      n_garden <- if (!is.null(df_garden)) nrow(df_garden) else 0
      n_butterfly <- if (file.exists(butterfly_path)) nrow(read.csv(butterfly_path)) else 0
      n_unique <- if (file.exists(unique_path)) nrow(read.csv(unique_path)) else 0

      counts <- data.frame(
        Type = c("Plant Sites", "Butterfly Sites", "Bird Sites"),
        Count = c(n_garden, n_butterfly, n_unique),
        Color = c("#66CDAA", "#cc99ff", "#b22222")
      )

      # --- Draw bar chart ---
      par(mar = c(3, 5, 2, 1))
      bar_positions <- barplot(
        counts$Count,
        names.arg = counts$Type,
        col = counts$Color,
        border = NA,
        horiz = TRUE,
        las = 1,
        xlab = "Count",
        xlim = c(0, max(counts$Count) * 1.2),
        cex.names = 0.8
      )

      # Label count at bar end
      text(counts$Count, bar_positions, labels = counts$Count, pos = 4, cex = 0.8)
    })


    # Tree click (left JS) → map: highlight circle + open same popup
    observeEvent(input$rtree_click_from_js, {
      nm <- input$rtree_click_from_js
      pts <- points_state()
      req(pts, nrow(pts) > 0, nm)

      # Can use display_name or species to match
      idx <- which(pts$display_name == nm)
      if (!length(idx) && "species" %in% names(pts)) idx <- which(pts$species == nm)
      if (!length(idx)) {
        return()
      }
      idx <- idx[1]

      lat <- as.numeric(pts$lat[idx])
      lon <- as.numeric(pts$lon[idx])

      leafletProxy("map", session) |>
        setView(lng = lon, lat = lat, zoom = 15) |>
        clearGroup("highlight") |>
        addCircleMarkers(
          lng = lon, lat = lat, radius = 10, color = "#004d40", fillColor = "#ffb300",
          fillOpacity = 0.9, weight = 2, group = "highlight",
          popup = paste0("<b>", htmltools::htmlEscape(nm), "</b>")
        ) |>
        clearPopups() |>
        addPopups(
          lng = lon, lat = lat,
          popup = htmltools::HTML(pts$popup_html[idx]),
          options = popupOptions(closeButton = TRUE, className = "bio-popup-outer", maxWidth = 360)
        )
    })
  })
}

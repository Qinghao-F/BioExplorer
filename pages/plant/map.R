# pages/plant/map.R — single-file version (map + radar + plantmap)

# Packages
library(shiny)
library(leaflet)
library(ggplot2)
library(readr)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(tools)

# Paths
PLANT_BASE <- rprojroot::find_root(rprojroot::has_file("app.R"))

# A Leaflet module for Plant page: expects data.frame(lon, lat, name, category, address?, value?)
plantmap_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), width = "100%", height = "100%")
}

plantmap_server <- function(id, data_reactive, popup_reactive = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_id <- reactiveVal(NULL)
    
    output$map <- renderLeaflet({
      df <- data_reactive()
      if (is.null(df) || !nrow(df)) {
        leaflet() |>
          addProviderTiles(providers$CartoDB.DarkMatter) |>
          setView(lng = 144.9631, lat = -37.8136, zoom = 11)
      } else {
        # Calculate the boundary
        lon_min <- min(df$lon, na.rm = TRUE); lon_max <- max(df$lon, na.rm = TRUE)
        lat_min <- min(df$lat, na.rm = TRUE); lat_max <- max(df$lat, na.rm = TRUE)
        
        # Icon path
        icon_url <- paste0("icons/", tolower(df$category), ".svg")
        row_icons <- leaflet::icons(
          iconUrl   = icon_url,
          iconWidth = 40,
          iconHeight= 40
        )
        
        # Pop-up window
        pop <- popup_reactive()
        if (is.null(pop) || length(pop) != nrow(df)) pop <- htmltools::htmlEscape(df$name)
        
        m <- leaflet(df) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addMarkers(
            lng = ~lon, lat = ~lat,
            label = ~name,
            popup = pop,
            icon  = row_icons,
            layerId = ~name,
            group = ~category
          ) |>
          fitBounds(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max)
        
        # Static legend
        cats <- sort(unique(df$category))
        panel_html <- htmltools::tags$div(
          id = ns("layerlegend"),
          style = paste(
            "pointer-events:none;",
            "background:#FFFFFF;",
            "padding:14px 16px;",
            "border-radius:14px;",
            "box-shadow:0 2px 10px rgba(0,0,0,.25);",
            "font-size:16px; line-height:1.25;",
            "user-select:none;"
          ),
          htmltools::tags$div(
            style="font-weight:800; font-size:18px; margin-bottom:8px; color:#1b3a57;",
            "Type"
          ),
          lapply(cats, function(cat) {
            src <- paste0("icons/", tolower(cat), ".svg")
            htmltools::tags$div(
              style = "display:flex; align-items:center; gap:10px; margin:8px 0;",
              htmltools::tags$img(src = src, width = 26, height = 26, style="flex:0 0 26px;"),
              htmltools::tags$span(style="font-weight:600; color:#1b3a57;", cat)
            )
          })
        )
        
        m <- m |>
          addControl(html = htmltools::as.tags(panel_html), position = "topright") |>
          htmlwidgets::onRender(
            sprintf(
              "function(el,x){
         var legend = el.querySelector('#%s');
         if(!legend) return;
         var wrap = legend.parentNode;
         if(wrap){
           wrap.style.pointerEvents = 'none';
           wrap.style.background = 'transparent';
           wrap.style.border = '0';
         }
       }",
              ns("layerlegend")
            )
          )
      }
    })
    
    # highlight
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click$id)) return()
      selected_id(click$id)
      
      df <- data_reactive()
      row <- df[df$name == click$id, , drop = FALSE]
      if (!nrow(row)) return()
      
      leafletProxy(ns("map")) |>
        clearGroup("highlight") |>
        addCircleMarkers(
          lng = row$lon, lat = row$lat,
          radius = 14,
          stroke = TRUE, weight = 3, opacity = 1,
          color = "#26833E", fillColor = "#26833E", fillOpacity = 0.35,
          group = "highlight",
          options = pathOptions(pane = "markerPane")
        )
    })
    
    # Cancel highlighting
    observeEvent(input$map_click, {
      selected_id(NULL)
      leafletProxy(ns("map")) |> clearGroup("highlight")
    })
    
    return(list(selected = reactive(selected_id())))
  })
}

# Radar
# Richness / Abundance / Balance / Evenness / Coverage

site_radar_ui <- function(id, height = "45vh") {
  ns <- NS(id)
  tagList(
    div(
      style="margin:2px 0 6px 0;",
      plotOutput(ns("radar_plot"), height = height)
    ),
    div(
      style = "font-size:12px; line-height:1.35; color:#5c6f82; margin:2px 2px 10px 2px;",
      tags$b("What do these mean? "),
      HTML(paste(
        "<ul style='margin:6px 0 0 18px; padding:0;'>",
        "<li><b>Richness</b>: number of distinct plant species at the site.</li>",
        "<li><b>Abundance</b>: total observation records.</li>",
        "<li><b>Balance</b>: 1 − Top species dominance (lower dominance → higher balance).</li>",
        "<li><b>Evenness</b>: how evenly records are distributed across species.</li>",
        "<li><b>Coverage</b>: relative area (if available) or a proxy using abundance.</li>",
        "</ul>"
      ))
    )
  )
}

site_radar_server <- function(id, site_name_reactive, plant_df_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    norm_str <- function(x) tolower(trimws(x))
    
    all_sites_metrics <- reactive({
      pdat <- plant_df_reactive()
      if (is.null(pdat) || !nrow(pdat)) return(NULL)
      
      has_col <- function(cn) cn %in% names(pdat)
      botanical_col <- if (has_col("Botanical name")) "Botanical name" else {
        hit <- grep("botanical", names(pdat), ignore.case = TRUE, value = TRUE)
        if (length(hit)) hit[1] else NULL
      }
      site_col <- {
        cands <- c("site","name","location","garden","park","site_name","place","venue","area")
        hit <- names(pdat)[match(tolower(cands), tolower(names(pdat)))]
        hit[!is.na(hit)][1]
      }
      area_col <- {
        hit <- grep("^area\\b|\\barea\\b|\\(m2\\)", names(pdat), ignore.case = TRUE, value = TRUE)
        if (length(hit)) hit[1] else NULL
      }
      if (is.null(site_col) || is.null(botanical_col)) return(NULL)
      
      dat <- pdat
      dat$.site  <- norm_str(as.character(dat[[site_col]]))
      dat$.plant <- norm_str(as.character(dat[[botanical_col]]))
      dat <- dat[nzchar(dat$.site) & nzchar(dat$.plant), , drop = FALSE]
      if (!nrow(dat)) return(NULL)
      
      by_site <- split(dat, dat$.site)
      raw_list <- lapply(by_site, function(d) {
        richness   <- length(unique(d$.plant))
        abundance  <- nrow(d)
        tab <- sort(table(d$.plant), decreasing = TRUE)
        top_share <- if (length(tab)) as.numeric(tab[1]) / sum(tab) else 0
        balance   <- 1 - top_share
        S <- richness
        H <- if (length(tab)) {
          p <- as.numeric(tab) / sum(tab)
          -sum(p * log(p))
        } else 0
        evenness <- if (S > 1) H / log(S) else 0
        cover_raw <- if (!is.null(area_col) && area_col %in% names(d)) {
          suppressWarnings(sum(as.numeric(d[[area_col]]), na.rm = TRUE))
        } else {
          abundance
        }
        data.frame(
          site_key = unique(d$.site)[1],
          richness = richness,
          abundance = abundance,
          balance  = balance,
          evenness = evenness,
          cover    = cover_raw
        )
      })
      raw <- do.call(rbind, raw_list)
      
      # Normalization（0–100）
      norm01 <- function(x){
        rng <- range(x, na.rm = TRUE)
        if (!is.finite(rng[1]) || diff(rng) == 0) return(rep(0.0, length(x)))
        (x - rng[1]) / (rng[2] - rng[1])
      }
      score_count <- function(x) 100 * norm01(log1p(x))
      
      out <- within(raw, {
        Richness  <- score_count(richness)
        Abundance <- score_count(abundance)
        Balance   <- 100 * norm01(balance)
        Evenness  <- 100 * evenness
        Coverage  <- score_count(cover)
      })
      out[, c("site_key","Richness","Abundance","Balance","Evenness","Coverage")]
    })
    
    current_metrics <- reactive({
      s <- site_name_reactive()
      allm <- all_sites_metrics()
      if (is.null(s) || is.null(allm)) return(NULL)
      key <- norm_str(s)
      row <- allm[allm$site_key == key, , drop = FALSE]
      if (!nrow(row)) return(NULL)
      vals <- as.numeric(row[1, c("Richness","Abundance","Balance","Evenness","Coverage")])
      nms  <- c("Richness","Abundance","Balance","Evenness","Coverage")
      keep <- is.finite(vals)
      list(values = vals[keep], labels = nms[keep])
    })
    
    output$radar_plot <- renderPlot({
      par(bg = NA)
      cm <- current_metrics()
      if (is.null(cm) || length(cm$values) < 3) {
        par(mar = c(0,0,0,0)); plot.new()
        text(0.5, 0.5, labels = "Not enough data for radar", col = "#6c757d")
        return(invisible())
      }
      vals <- cm$values
      labs <- cm$labels
      k    <- length(vals)
      
      ang <- seq(0, 2*pi, length.out = k + 1)[- (k+1)]
      x <- vals * cos(ang)
      y <- vals * sin(ang)
      
      par(mar = c(4.2, 4.2, 4.2, 4.2), xaxs = "i", yaxs = "i")
      plot(NA, xlim = c(-140, 140), ylim = c(-140, 140),
           axes = FALSE, xlab = "", ylab = "", asp = 1)
      
      grid_levels <- c(20,40,60,80,100)
      for (g in grid_levels) {
        th <- seq(0, 2*pi, length.out = 181)
        lines(g*cos(th), g*sin(th),
              col = rgb(27/255,58/255,87/255, 0.12), lwd = 1.3)
      }
      for (i in seq_along(ang)) {
        segments(0, 0, 100*cos(ang[i]), 100*sin(ang[i]),
                 col = rgb(27/255,58/255,87/255, 0.26), lwd = 1.3)
      }
      
      op <- par(xpd = NA)
      label_r <- 130
      for (i in seq_along(ang)) {
        text(label_r*cos(ang[i]), label_r*sin(ang[i]),
             labels = labs[i], cex = 1.05)
      }
      par(op)
      
      polygon(c(x, x[1]), c(y, y[1]),
              border = "#26833E",
              col = rgb(38/255,131/255,62/255, 0.30),
              lwd = 3.2)
      points(x, y, pch = 21, bg = "#26833E", col = "#1b3a57", cex = 1.45, lwd = 1.2)
    }, bg = "transparent")
  })
}

#  UI + Server
plant_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeCSS(file.path(PLANT_BASE, "assets/bio.css")),
    
    # layout
    tags$style(HTML("
      #__plant_root__{height:95vh;}
      #__plant_root__ .bio-side-panel,
      #__plant_root__ .bio-map-container{height:(95%;}
      #__plant_root__ .bio-side-panel{overflow-y:auto;}
      #__plant_root__ .leaflet,
      #__plant_root__ .leaflet-container{height:95% !important;}

      .info-line{
        display:block; margin-top:6px;
        font-weight:700; color:#1b3a57;
      }
      .info-line a{ color:#155a9d; font-weight:700; text-decoration:underline; }
    ")),
    tags$style(HTML("
      /* Top species button */
      .bio-side-panel ul.bio-species-list{
        list-style:none !important;
        padding:0 !important; margin:8px 0 0 0 !important;
        display:flex; flex-wrap:wrap; gap:10px;
      }
      .bio-side-panel ul.bio-species-list li{ list-style:none !important; }

      .bio-side-panel a.bio-species-btn{
        display:inline-flex; align-items:center; gap:6px;
        padding:8px 12px;
        border:1.6px solid #1b3a57; border-radius:999px;
        background:#fff; color:#1b3a57 !important; text-decoration:none !important;
        font-weight:600; line-height:1.1;
        box-shadow:0 1px 2px rgba(0,0,0,.06);
        transition:background .15s ease, border-color .15s ease, transform .02s ease;
        cursor:pointer;
      }
      .bio-side-panel a.bio-species-btn:hover{
        background:#e8f5ef; border-color:#0e6b50;
      }
      .bio-side-panel a.bio-species-btn:active{ transform:scale(0.98); }
      .bio-side-panel a.bio-species-btn em{ font-style:italic; }
      .bio-side-panel a.bio-species-btn .leaf{ font-size:14px; line-height:1; }
    ")),
    tags$style(HTML("
      /* Basic styles of suspended Windows */
      .species-float {
        position:absolute; right:18px; top:18px; z-index: 1000;
        width:min(38vw, 520px); max-height:72vh; overflow:auto;
        background:rgba(255,255,255,.94);
        backdrop-filter: blur(6px);
        border:1px solid rgba(27,58,87,.18);
        border-radius:14px;
        box-shadow:0 10px 30px rgba(0,0,0,.18);
        color:#1b3a57;
      }
      .species-float .head{
        display:flex; align-items:center; justify-content:space-between;
        padding:10px 12px; border-bottom:1px solid rgba(27,58,87,.12);
        position:sticky; top:0; background:rgba(255,255,255,.96);
        backdrop-filter: blur(6px); z-index:1;
      }
      .species-float .body{ padding:12px; }
      .species-close-btn{
        border:none; background:transparent; cursor:pointer; font-size:18px; line-height:1;
        color:#1b3a57; opacity:.7;
      }
      .species-close-btn:hover{ opacity:1; transform: scale(1.05); }
    ")),
    tags$style(HTML("
      @media (max-width: 1200px){
        #__plant_root__ .bio-side-panel{ width:42%; }
        #__plant_root__ .bio-map-container{ width:58%; }
      }
      @media (max-width: 992px){
        #__plant_root__{ flex-direction:column; }
        #__plant_root__ .bio-side-panel,
        #__plant_root__ .bio-map-container{ width:100%; }
        #__plant_root__ .bio-map-container{ height:55vh; }
      }
    ")),
    
    includeScript(file.path(PLANT_BASE, "assets/bio.js")),
    
    div(
      id = "__plant_root__",
      class = "bio-layout",
      style = "display:flex; flex-direction:row; width:100%; margin:0; padding:0; overflow:hidden; border:none; position:relative;",
      
      # left side bar
      div(
        class = "bio-side-panel",
        style = "width:40%; padding:16px; display:flex; flex-direction:column;",
        h3("Plants in Melbourne",
           style = "margin-bottom:16px; font-weight:700; color:#1b3a57;"
        ),
        div(
          id = ns("overview_wrap"),
          uiOutput(ns("overview_panel"))
        )
      ),
      
      # map
      div(
        class = "bio-map-container",
        style = "width:60%; margin:0; padding:0; border:none; box-sizing:border-box;",
        plantmap_ui(ns("plantmap"))
      ),
      
      uiOutput(ns("species_floating"))
    )
  )
}

plant_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    htmlEscape <- htmltools::htmlEscape
    normalize_key <- function(x) tolower(trimws(x))
    
    # Google Maps link
    gmaps_url <- function(addr, lon, lat, name) {
      addr <- if (is.null(addr) || is.na(addr)) "" else trimws(as.character(addr))
      name <- if (is.null(name) || is.na(name)) "" else trimws(as.character(name))
      has_addr <- nzchar(addr)
      has_coords <- is.finite(as.numeric(lon)) && is.finite(as.numeric(lat))
      
      if (has_addr) {
        q <- utils::URLencode(addr, reserved = TRUE)
        return(paste0("https://www.google.com/maps/search/?api=1&query=", q))
      }
      if (has_coords) {
        return(paste0("https://www.google.com/maps/search/?api=1&query=",
                      sprintf("%s,%s", as.numeric(lat), as.numeric(lon))))
      }
      if (nzchar(name)) {
        q <- utils::URLencode(name, reserved = TRUE)
        return(paste0("https://www.google.com/maps/search/?api=1&query=", q))
      }
      NA_character_
    }
    
    # Base point
    raw <- suppressMessages(
      readr::read_csv("dataset/garden_site.csv", show_col_types = FALSE, progress = FALSE)
    )
    
    df <- raw |>
      dplyr::transmute(
        name     = trimws(as.character(.data$Site)),
        category = tools::toTitleCase(trimws(as.character(.data$Category))),
        lon      = as.numeric(.data$Lon),
        lat      = as.numeric(.data$Lat),
        address  = trimws(as.character(.data$Address))
      ) |>
      dplyr::filter(is.finite(lon), is.finite(lat), nzchar(name)) |>
      dplyr::distinct(name, .keep_all = TRUE)
    
    # Map pop-up
    popup_name_only <- if (nrow(df)) htmltools::htmlEscape(df$name) else character(0)
    
    # Map module
    map_ret <- plantmap_server(
      "plantmap",
      data_reactive  = reactive(df),
      popup_reactive = reactive(popup_name_only)
    )
    
    # Plant data
    top3_by_site <- NULL
    rawp <- NULL
    if (file.exists("dataset/plant.csv")) {
      rawp <- suppressMessages(readr::read_csv("dataset/plant.csv", show_col_types = FALSE, progress = FALSE, guess_max = 100000))
      if (nrow(rawp)) {
        plant_col <- "Botanical name"
        if (!(plant_col %in% names(rawp))) {
          stop("Can't fint col in dataset/plant.csv：", plant_col)
        }
        pick_texty_col <- function(dat, prefer, also = prefer) {
          cols <- names(dat); lc <- tolower(cols)
          hit <- which(lc %in% tolower(prefer))
          cands <- if (length(hit)) cols[hit] else character(0)
          if (!length(cands)) {
            ptn <- paste0(also, collapse="|")
            hit2 <- which(grepl(ptn, lc, ignore.case = TRUE))
            cands <- cols[hit2]
          }
          if (!length(cands)) return(NA_character_)
          score_one <- function(v){
            v <- trimws(as.character(v)); v <- v[nzchar(v)]
            if (!length(v)) return(-Inf)
            is_pct <- grepl("^\\d+\\s*%$|^\\d+\\s*-\\s*\\d+\\s*%$", v)
            is_num <- suppressWarnings(!is.na(as.numeric(v)))
            txt <- !(is_pct | is_num)
            3*mean(txt) + 2*(length(unique(v[txt]))/max(1,length(v)))
          }
          sc <- sapply(cands, function(cn) score_one(dat[[cn]]))
          cands[which.max(sc)]
        }
        site_col <- pick_texty_col(
          rawp,
          prefer = c("site","name","location","garden","park","site_name","place","venue","area")
        )
        if (!is.na(site_col)) {
          p2 <- rawp |>
            dplyr::mutate(
              .site  = trimws(as.character(.data[[site_col]])),
              .plant = trimws(as.character(.data[[plant_col]]))
            ) |>
            dplyr::filter(.site != "", .plant != "")
          norm <- function(x) tolower(trimws(x))
          p2 <- dplyr::mutate(p2, .site_key = norm(.site))
          df  <- dplyr::mutate(df,  .site_key = norm(name))
          counts <- p2 |>
            dplyr::count(.site_key, .plant, name = "n") |>
            dplyr::arrange(.site_key, dplyr::desc(n), .plant)
          top3 <- counts |>
            dplyr::group_by(.site_key) |>
            dplyr::slice_head(n = 3) |>
            dplyr::ungroup()
          top3_by_site <- split(top3[, c(".plant","n")], f = top3$.site_key)
        }
      }
    }
    
    # Species richness
    rich_top10 <- reactive({
      if (is.null(rawp) || !nrow(rawp)) return(NULL)
      botanical_col <- "Botanical name"
      if (!(botanical_col %in% names(rawp))) return(NULL)
      
      pick_texty_col <- function(dat, prefer, also = prefer) {
        cols <- names(dat); lc <- tolower(cols)
        hit <- which(lc %in% tolower(prefer))
        cands <- if (length(hit)) cols[hit] else character(0)
        if (!length(cands)) {
          ptn <- paste0(also, collapse="|")
          hit2 <- which(grepl(ptn, lc, ignore.case = TRUE))
          cands <- cols[hit2]
        }
        if (!length(cands)) return(NA_character_)
        score_one <- function(v){
          v <- trimws(as.character(v)); v <- v[nzchar(v)]
          if (!length(v)) return(-Inf)
          is_pct <- grepl("^\\d+\\s*%$|^\\d+\\s*-\\s*\\d+\\s*%$", v)
          is_num <- suppressWarnings(!is.na(as.numeric(v)))
          txt <- !(is_pct | is_num)
          3*mean(txt) + 2*(length(unique(v[txt]))/max(1,length(v)))
        }
        sc <- sapply(cands, function(cn) score_one(dat[[cn]]))
        cands[which.max(sc)]
      }
      
      site_col <- pick_texty_col(
        rawp,
        prefer = c("site","name","location","garden","park","site_name","place","venue","area")
      )
      if (is.na(site_col)) return(NULL)
      
      rp <- rawp |>
        dplyr::transmute(
          site  = trimws(as.character(.data[[site_col]])),
          plant = trimws(as.character(.data[[botanical_col]]))
        ) |>
        dplyr::filter(site != "", plant != "") |>
        dplyr::distinct(site, plant) |>
        dplyr::count(site, name = "richness") |>
        dplyr::arrange(dplyr::desc(richness)) |>
        dplyr::slice_head(n = 10)
      rp
    })
    
    output$rich_bar <- renderPlot({
      dat <- rich_top10()
      validate(need(!is.null(dat) && nrow(dat) > 0,
                    "No species richness data available yet. Please load dataset/plant.csv."))
      
      dat$site <- factor(dat$site, levels = dat$site[order(dat$richness, decreasing = TRUE)])
      
      ggplot2::ggplot(dat, ggplot2::aes(x = site, y = richness)) +
        ggplot2::geom_col(width = 0.7, fill = "#26833E") +
        ggplot2::geom_text(ggplot2::aes(label = richness), vjust = -0.4, size = 3.8) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(
          title = "Gardens by species richness",
          x = NULL,
          y = "Number of species"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 6)),
          axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1, color = "#1b3a57"),
          axis.text.y = ggplot2::element_text(color = "#1b3a57"),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color = "#E6E6E6"),
          panel.grid.minor.y = ggplot2::element_line(color = "#E6E6E6"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          legend.background= ggplot2::element_rect(fill = "transparent", colour = NA)
        )
    }, bg = "transparent", res = 110)
    
    # Rader map
    plant_df_reactive <- reactive({
      if (!is.null(rawp) && nrow(rawp)) rawp else NULL
    })
    
    site_radar_server(
      id = "radar",
      site_name_reactive = reactive(map_ret$selected()),
      plant_df_reactive  = plant_df_reactive
    )
    
    # Species information lookup table
    species_info_lookup <- NULL
    read_species_csv <- function(path) {
      suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, locale = readr::locale(encoding = "UTF-8"), guess_max = 100000))
    }
    
    if (file.exists("dataset/species_info.csv")) {
      sp <- read_species_csv("dataset/species_info.csv")
      want_cols <- c("Botanical name","Common name","Family","Habit","Status","Description","Image","Source")
      keep <- intersect(want_cols, names(sp))
      if (!("Botanical name" %in% keep)) stop("species_info.csv 缺少必须列：Botanical name")
      sp <- dplyr::distinct(sp[, keep, drop = FALSE], `Botanical name`, .keep_all = TRUE)
      sp$.key <- normalize_key(sp[["Botanical name"]])
      species_info_lookup <- split(sp, sp$.key)
    } else if (!is.null(rawp)) {
      pick <- function(cands) {
        hits <- intersect(cands, names(rawp))
        if (length(hits)) hits[1] else {
          rx <- grep(paste(cands, collapse="|"), names(rawp), ignore.case = TRUE, value = TRUE)
          if (length(rx)) rx[1] else NA_character_
        }
      }
      botanical_col <- "Botanical name"
      common_col    <- pick(c("Common name","Common_name","Common","Vernacular"))
      family_col    <- pick(c("Family"))
      desc_col      <- pick(c("Description","Notes","Note","Summary","Info","Information","Detail","Details"))
      habit_col     <- pick(c("Habit","Life form","Form"))
      status_col    <- pick(c("Status","Native/Exotic","Origin","Type"))
      keep_cols <- c(botanical_col, common_col, family_col, desc_col, habit_col, status_col)
      keep_cols <- keep_cols[!is.na(keep_cols)]
      sp <- unique(rawp[, keep_cols, drop = FALSE])
      sp$.key <- normalize_key(sp[[botanical_col]])
      species_info_lookup <- split(sp, sp$.key)
    }
    
    # Suspended species panel
    selected_species <- reactiveVal(NULL)
    show_species_panel <- reactiveVal(FALSE)
    
    output$species_floating <- renderUI({
      req(show_species_panel())
      div(class="species-float",
          div(class="head",
              tags$strong("Species"),
              actionButton(ns("species_close"), label = HTML("&times;"), class = "species-close-btn")
          ),
          div(class="body",
              uiOutput(ns("species_panel"))
          )
      )
    })
    observeEvent(input$species_close, { show_species_panel(FALSE) })
    
    # Overview
    output$overview_panel <- renderUI({
      id <- map_ret$selected()
      if (is.null(id)) {
        return(div(style="opacity:.85;",
                   tags$h4("Select a garden on the map 👉"),
                   p("Click any marker to see the overview here."),
                   plotOutput(ns("rich_bar"), height = "500px")
        ))
      }
      row <- df[df$name == id, , drop = FALSE]; if (!nrow(row)) return(NULL)
      
      addr_line <- if (!is.na(row$address) && nzchar(row$address)) {
        span(style="display:block; margin-top:2px; opacity:.9;",
             HTML(paste0("📍 ", htmlEscape(row$address))))
      } else NULL
      
      dest <- if (!is.na(row$address) && nzchar(row$address)) row$address
      else sprintf("%.6f,%.6f", row$lat[1], row$lon[1])
      gmaps_url_line <- paste0(
        "https://www.google.com/maps/dir/?api=1&destination=",
        utils::URLencode(dest, reserved = TRUE),
        "&travelmode=transit"
      )
      transit_line <- span(
        class = "info-line",
        HTML(sprintf("🚌 Transport/Directions: <a href=\"%s\" target=\"_blank\" rel=\"noopener\">View on Google Maps.</a>", gmaps_url_line))
      )
      
      # Top species
      norm <- function(x) tolower(trimws(x))
      site_key <- norm(row$name[1])
      
      if (!is.null(top3_by_site) && site_key %in% names(top3_by_site)) {
        t3 <- as.data.frame(top3_by_site[[site_key]])
        n_show <- min(3, nrow(t3))
        
        lis <- lapply(seq_len(n_show), function(i) {
          sp <- as.character(t3$.plant[i])
          link_id <- ns(paste0("sp_link_", i))
          tags$li(
            actionLink(
              inputId = link_id,
              label   = tagList(span(class = "leaf", "🌿"), em(htmlEscape(sp))),
              class   = "action-button bio-species-btn"
            )
          )
        })
        
        top_species_ui <- tagList(
          span(class = "info-line", HTML("🌿 Top species")),
          tags$ul(class = "bio-species-list", lis)
        )
      } else {
        top_species_ui <- div(tags$small(HTML("<em>No species data for this site.</em>")))
      }
      
      tagList(
        div(
          style = "background:transparent; color:#1b3a57; padding:0; border-radius:0; box-shadow:none;",
          h4(htmlEscape(row$name), style="margin:0 0 6px; color:#1b3a57;"),
          addr_line,
          transit_line,
          tags$hr(style = "border:0; border-top:1px solid rgba(27,58,87,0.18); margin:10px 10px 10px 0;"),
          site_radar_ui(ns("radar"), height = "48vh"),
          top_species_ui
        )
      )
    })
    
    # Listen for the Top species button
    observe({
      id <- map_ret$selected(); req(id)
      site_key <- tolower(trimws(df[df$name == id, , drop = FALSE]$name[1]))
      sp_vec <- if (!is.null(top3_by_site) && site_key %in% names(top3_by_site)) {
        as.character(head(as.data.frame(top3_by_site[[site_key]])$.plant, 3))
      } else character(0)
      
      lapply(seq_along(sp_vec), function(i){
        observeEvent(input[[paste0("sp_link_", i)]], {
          selected_species(sp_vec[i])
          show_species_panel(TRUE)
        }, ignoreInit = TRUE)
      })
    })
    
    observeEvent(show_species_panel(), {
      if (isTRUE(show_species_panel()) && is.null(selected_species())) {
        id <- map_ret$selected(); req(id)
        site_key <- tolower(trimws(df[df$name == id, , drop = FALSE]$name[1]))
        if (!is.null(top3_by_site) && site_key %in% names(top3_by_site)) {
          t3 <- as.data.frame(top3_by_site[[site_key]])
          if (nrow(t3) >= 1) selected_species(as.character(t3$.plant[1]))
        }
      }
    }, ignoreInit = TRUE)
    
    observeEvent(map_ret$selected(), {
      if (isTRUE(show_species_panel())) {
        show_species_panel(FALSE)
        selected_species(NULL)
      }
    }, ignoreInit = TRUE)
    
    # rander img
    render_img <- function(src) {
      if (is.null(src) || is.na(src)) return(NULL)
      src <- trimws(src); if (!nzchar(src)) return(NULL)
      if (grepl("^https?://", src, ignore.case = TRUE)) {
        return(tags$img(src = src,
                        style = "max-width:100%; height:auto; border-radius:10px; display:block; margin:10px 0;"))
      }
      src_clean <- sub("^\\./", "", src)
      src_clean <- sub("^/*www/", "", src_clean)
      abs1 <- file.path(PLANT_BASE, src_clean)
      abs2 <- file.path(PLANT_BASE, "www", src_clean)
      if (!file.exists(abs1) && !file.exists(abs2)) {
        return(tags$img(src = src_clean,
                        style = "max-width:100%; height:auto; border-radius:10px; display:block; margin:10px 0;"))
      }
      tags$img(src = src_clean,
               style = "max-width:100%; height:auto; border-radius:10px; display:block; margin:10px 0;")
    }
    
    output$species_panel <- renderUI({
      sp <- selected_species()
      if (is.null(sp)) {
        return(div(style="opacity:.8;",
                   tags$h4("Click a species in Overview"),
                   p("The description of that species will appear here.")))
      }
      key <- normalize_key(sp)
      info <- if (!is.null(species_info_lookup) && key %in% names(species_info_lookup)) species_info_lookup[[key]] else NULL
      
      get1 <- function(df, col) if (!is.null(df) && col %in% names(df)) as.character(df[[col]][1]) else NA_character_
      botanical <- sp
      common    <- get1(info, "Common name")
      family    <- get1(info, "Family")
      habit     <- get1(info, "Habit")
      status    <- {
        cands <- c("Status","Native/Exotic","Origin","Type")
        hit <- cands[cands %in% names(info)]
        if (length(hit)) as.character(info[[hit[1]]][1]) else NA_character_
      }
      desc      <- {
        cands <- c("Description","Notes","Note","Summary","Info","Information","Detail","Details")
        hit <- cands[cands %in% names(info)]
        if (length(hit)) as.character(info[[hit[1]]][1]) else NA_character_
      }
      image_src <- get1(info, "Image")
      source_url<- get1(info, "Source")
      
      div(
        style = "background:transparent; color:#1b3a57; padding:0; border-radius:0; box-shadow:none;",
        h4(HTML(sprintf("<em>%s</em>", htmlEscape(botanical))), style="margin:0 0 8px;"),
        if (!is.na(common) && nzchar(common)) div(tags$b("Common name: "), htmlEscape(common)),
        if (!is.na(family) && nzchar(family)) div(tags$b("Family: "), htmlEscape(family)),
        if (!is.na(habit)  && nzchar(habit))  div(tags$b("Habit: "),  htmlEscape(habit)),
        if (!is.na(status) && nzchar(status)) div(tags$b("Status: "), htmlEscape(status)),
        tags$hr(style = "border:0; border-top:1px solid rgba(27,58,87,0.18); margin:10px 0;"),
        if (!is.na(image_src) && nzchar(image_src)) render_img(image_src),
        if (!is.na(desc) && nzchar(desc)) div(desc) else em("No description available for this species."),
        if (!is.na(source_url) && nzchar(source_url))
          div(style="margin-top:8px;", tags$a(href=source_url, target="_blank", "Source"))
      )
    })
  })
}


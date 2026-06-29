# pages/biodiversity/map.R
# Month filter from ALL rows; map uses rows with lon+lat; robust delimiter + column handling

library(shiny)
library(leaflet)

BIO_BASE <- rprojroot::find_root(rprojroot::has_file("app.R"))
source(file.path(BIO_BASE, "pages/biodiversity/module/left_sidebar_table1.R"))
# source(file.path(BIO_BASE, "pages/biodiversity/module/left_sidebar_table2.R"))  # Removed
source(file.path(BIO_BASE, "pages/biodiversity/module/biomap_helpers.R"))
source(file.path(BIO_BASE, "pages/biodiversity/module/biomap.R"))
source(file.path(BIO_BASE, "pages/biodiversity/module/right_sidebar.R"))

biodiversity_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeCSS(file.path(BIO_BASE, "assets/bio.css")),
    includeCSS(file.path(BIO_BASE, "assets/bio_right_sidebar.css")),
    includeCSS(file.path(BIO_BASE, "assets/marker_appear.css")),
    includeScript(file.path(BIO_BASE, "assets/bio.js")),

    # ==== Hide buttons above left_sidebar_table1 ====
    # Note: also hide actionButton / actionLink (both btn/link covered)
    tags$style(HTML(sprintf('
      /* Only applies to left sidebar on this page: limited to left_table1 namespace prefix */
      button[id^="%s-"],
      a.btn[id^="%s-"],
      .btn[id^="%s-"],
      .leftbar-toolbar, .left-toolbar, .lsb-toolbar {
        display: none !important;
      }
    ', ns("left_table1"), ns("left_table1"), ns("left_table1")))),
    div(
      id = "__bio_root__",
      class = "bio-layout",
      style = "display:flex; flex-direction:row; width:100%; margin:0; padding:0; overflow:hidden; border:none; position:relative;",

      # Left side (table1 only)
      div(
        class = "bio-side-panel",
        style = "width:50%; height:100%; padding:16px; display:flex; flex-direction:column;",
        h3("Biodiversity in Melbourne",
          style = "margin-bottom:16px; font-weight:700; color:#004d40;"
        ),
        # Place table1 directly, no longer use tabsetPanel/Statistics
        left_sidebar_table1_ui(ns("left_table1"))
      ),
      div(class = "bio-divider"),

      # Right side map + floating month filter (modular)
      div(
        class = "bio-map-container",
        style = "position:relative; z-index:2; width:70%; height:100%; margin:0; padding:0; border:none; box-sizing:border-box;",
        biomap_ui(ns("biomap")),
        div(class = "floating-panel", right_sidebar_ui(ns("right_sidebar")))
      )
    )
  )
}

biodiversity_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- Utility: remove BOM + normalize column names ----------
    strip_bom <- function(x) sub("^\ufeff", "", x)
    norm_name <- function(x) {
      x <- strip_bom(x)
      x <- tolower(trimws(x))
      gsub("[^a-z0-9]+", "", x)
    }

    # ---------- Utility: auto-detect delimiter and read (CSV / TSV / semicolon) ----------
    smart_read <- function(path) {
      first <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
      c_commas <- lengths(regmatches(first, gregexpr(",", first)))
      c_tabs <- lengths(regmatches(first, gregexpr("\t", first)))
      c_semi <- lengths(regmatches(first, gregexpr(";", first)))
      sep <- if (c_tabs > c_commas && c_tabs >= c_semi) "\t" else if (c_semi > c_commas) ";" else ","
      df <- tryCatch(
        utils::read.table(path,
          header = TRUE, sep = sep, quote = "\"",
          comment.char = "", stringsAsFactors = FALSE,
          check.names = FALSE, fill = TRUE, na.strings = c("", "NA")
        ),
        error = function(e) data.frame()
      )
      df
    }

    # ---------- Robust month extraction (YYYY-MM-DD direct substr) ----------
    extract_months <- function(x) {
      s <- trimws(as.character(x))
      m <- suppressWarnings(as.integer(substr(s, 6, 7)))
      need <- is.na(m)
      if (any(need)) {
        d <- suppressWarnings(as.Date(s[need]))
        bad <- is.na(d)
        if (any(bad)) {
          d[bad] <- suppressWarnings(as.Date(
            s[need][bad],
            tryFormats = c(
              "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d",
              "%d/%m/%Y", "%m/%d/%Y",
              "%d-%m-%Y", "%m-%d-%Y"
            )
          ))
        }
        m[need] <- suppressWarnings(as.integer(format(d, "%m")))
      }
      m[!(m >= 1 & m <= 12)] <- NA_integer_
      m
    }

    # ---------- Read + normalize (don't call global read_and_normalize_points) ----------
    load_df_all <- function(path) {
      if (!file.exists(path)) {
        return(data.frame())
      }
      df0 <- smart_read(path)
      if (!nrow(df0) || !ncol(df0)) {
        return(data.frame())
      }

      raw_names <- names(df0)
      names(df0) <- nn <- norm_name(raw_names)

      pick_idx <- function(cands) {
        cands <- norm_name(cands)
        hit <- match(cands, nn, nomatch = 0)
        hit <- hit[hit > 0]
        if (length(hit)) hit[1] else NA_integer_
      }
      lon_i <- pick_idx(c("lon", "long", "longitude", "lng", "x"))
      lat_i <- pick_idx(c("lat", "latitude", "y"))
      date_i <- pick_idx(c("sightingdate", "date", "observationdate", "eventdate", "recordeddate"))
      if (is.na(date_i)) {
        for (j in seq_along(nn)) {
          v <- as.character(df0[[j]])
          if (any(grepl("^\\s*\\d{4}[-/]\\d{2}[-/]\\d{2}", v))) {
            date_i <- j
            break
          }
        }
      }
      if (is.na(date_i)) {
        return(data.frame())
      }

      get_chr_idx <- function(idx) if (!is.na(idx)) as.character(df0[[idx]]) else rep(NA_character_, nrow(df0))
      get_num_idx <- function(idx) if (!is.na(idx)) suppressWarnings(as.numeric(df0[[idx]])) else rep(NA_real_, nrow(df0))

      out <- data.frame(
        lon = get_num_idx(lon_i),
        lat = get_num_idx(lat_i),
        taxa = if ("taxa" %in% nn) as.character(df0[["taxa"]]) else NA_character_,
        kingdom = if ("kingdom" %in% nn) as.character(df0[["kingdom"]]) else NA_character_,
        phylum = if ("phylum" %in% nn) as.character(df0[["phylum"]]) else NA_character_,
        class = if ("class" %in% nn) as.character(df0[["class"]]) else NA_character_,
        order = if ("order" %in% nn) as.character(df0[["order"]]) else NA_character_,
        family = if ("family" %in% nn) as.character(df0[["family"]]) else NA_character_,
        genus = if ("genus" %in% nn) as.character(df0[["genus"]]) else NA_character_,
        species = if ("species" %in% nn) as.character(df0[["species"]]) else NA_character_,
        commonname = if ("commonname" %in% nn) as.character(df0[["commonname"]]) else NA_character_,
        identificationnotes = if ("identificationnotes" %in% nn) as.character(df0[["identificationnotes"]]) else NA_character_,
        dataresourcename = if ("dataresourcename" %in% nn) as.character(df0[["dataresourcename"]]) else NA_character_,
        sightingdate = trimws(get_chr_idx(date_i)),
        location = if ("location" %in% nn) as.character(df0[["location"]]) else NA_character_,
        geopoint = if ("geopoint" %in% nn) as.character(df0[["geopoint"]]) else NA_character_,
        stringsAsFactors = FALSE
      )

      out$sighting_month <- extract_months(out$sightingdate)

      nz <- function(x) ifelse(!is.na(x) & nzchar(x), x, "")
      out$display_name <- ifelse(nz(out$commonname) != "",
        out$commonname,
        ifelse(nz(out$species) != "", out$species,
          ifelse(nz(out$taxa) != "", out$taxa, "Unknown")
        )
      )
      out
    }

    # Read raw data (full) + map data (requires lat/lon)
    df_all <- load_df_all(file.path(BIO_BASE, "dataset/biodiversity.csv"))
    df_map <- if (nrow(df_all)) df_all[is.finite(df_all$lon) & is.finite(df_all$lat), , drop = FALSE] else df_all

    # Right sidebar filter module: month generated from df_all; map filtered with df_map #
    filtered_df <- right_sidebar_server(
      "right_sidebar",
      data_all = reactive(df_all),
      data_map = reactive(df_map)
    )

    # (Optional) Shared hook for other modules to access filtered data
    session$userData$bio_filtered_df <- filtered_df

    # Left side module (table1 only)
    left_sidebar_table1_server("left_table1", map_module_id = ns("biomap"))
    # left_sidebar_table2_server("left_table2")  # Removed

    # Map: use filtered data
    biomap_server("biomap", data_reactive = filtered_df)
  })
}

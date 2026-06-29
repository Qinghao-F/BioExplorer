# pages/biodiversity/module/left_sidebar_table1.R
library(shiny)
library(dplyr)
library(tidyr)
library(data.tree)
library(r2d3)
library(digest)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
nz <- function(x) ifelse(!is.na(x) & nzchar(x), x, "")

normalize_names <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  names(df) <- tolower(gsub("[^a-z0-9]+", "", names(df)))
  df
}

guess_levels <- function(df) {
  cand <- c("kingdom", "taxa", "phylum", "class", "order", "family", "genus", "species")
  lv <- cand[cand %in% names(df)]
  if (!length(lv)) stop("No taxonomy columns found.")
  lv
}

prepare_biodiv_tree <- function(df, levels = NULL, value_col = NULL, root = "Biodiversity") {
  df <- normalize_names(df)
  levels <- levels %||% guess_levels(df)
  df <- df %>% mutate(across(all_of(levels), ~ replace_na(as.character(.x), "Unknown")))
  agg <- if (is.null(value_col)) {
    df %>% count(across(all_of(levels)), name = "n")
  } else {
    df %>%
      group_by(across(all_of(levels))) %>%
      summarise(n = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  }
  agg <- agg %>%
    unite("pathString", all_of(levels), sep = "/", remove = FALSE) %>%
    mutate(pathString = paste(root, pathString, sep = "/"))
  tree <- data.tree::as.Node(agg)
  attr(tree, "levels") <- levels
  attr(tree, "root") <- root
  tree
}

# UID rule consistent with map
.make_uid <- function(species, common, lat, lon) {
  key <- paste0(
    nz(species), "|", nz(common), "|",
    sprintf("%.6f", as.numeric(lat)), "|", sprintf("%.6f", as.numeric(lon))
  )
  digest::digest(key, algo = "xxhash64")
}

left_sidebar_table1_ui <- function(id) {
  ns <- NS(id)
  # Outer relative positioning, provides reference for percentage height; min-height:0 fixes overflow in flex scenarios
  tags$div(
    id = ns("tree_container"),
    style = "position:relative; width:100%; height:100%; min-height:0;",
    # Inner absolute positioning, edges aligned <U+2192> fills left sidebar
    tags$div(
      id = ns("tree_wrap"),
      style = "position:absolute; inset:0; overflow:hidden; background:none;",
      r2d3::d3Output(ns("radial_tree"), height = "100%", width = "100%")
    )
  )
}

left_sidebar_table1_server <- function(
    id, biodiv = NULL, levels = NULL, value_col = NULL, root = "Biodiversity",
    map_module_id = "bioMap") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    biodiv_rx_raw <- if (!is.null(biodiv)) biodiv else if (!is.null(session$userData$bio_filtered_df)) session$userData$bio_filtered_df else reactive(NULL)

    biodiv_rx <- reactive({
      df <- biodiv_rx_raw()
      if (is.null(df) || !nrow(df)) {
        return(df)
      }
      normalize_names(df)
    })

    # Cache key
    tree_cache_key <- reactive({
      df <- biodiv_rx()
      req(nrow(df) > 0)
      levs <- levels %||% guess_levels(df)
      date_vec <- NULL
      if ("sightingdate" %in% names(df)) date_vec <- df[["sightingdate"]] else if ("sighting date" %in% names(df)) date_vec <- df[["sighting date"]]
      js_path <- "www/force_tree_expandable.js"
      js_mtime <- tryCatch(file.info(js_path)$mtime, error = function(e) Sys.time())
      digest::digest(list(df[, intersect(names(df), levs), drop = FALSE], date_vec, levs, root, js_mtime), algo = "xxhash64")
    })

    output$radial_tree <- r2d3::renderD3({
      df <- biodiv_rx()
      validate(need(nrow(df) > 0, "No data to draw."))
      levs <- levels %||% guess_levels(df)
      miss <- setdiff(levs, names(df))
      validate(need(!length(miss), paste("Missing columns:", paste(miss, collapse = ", "))))

      # 1) Tree
      tree <- prepare_biodiv_tree(df, levs, value_col, root)

      # 2) Inject month (take most recent sighting for each taxo combination)
      df_norm <- normalize_names(df)
      if ("sightingdate" %in% names(df_norm)) {
        suppressWarnings({
          dt <- as.Date(df_norm$sightingdate)
        })
      } else if ("sighting date" %in% names(df_norm)) {
        suppressWarnings({
          dt <- as.Date(df_norm[["sighting date"]])
        })
      } else {
        dt <- as.Date(NA)
      }
      df_norm$..dt <- dt

      month_map <- df_norm %>%
        dplyr::filter(!is.na(..dt)) %>%
        dplyr::group_by(dplyr::across(all_of(levs))) %>%
        dplyr::slice_max(order_by = ..dt, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(month = as.integer(format(..dt, "%m"))) %>%
        tidyr::unite("pathKey", dplyr::all_of(levs), sep = "/", remove = FALSE) %>%
        dplyr::select(pathKey, month)

      # 3) Inject uids (rule consistent with map)
      lat <- if ("lat" %in% names(df_norm)) as.numeric(df_norm$lat) else if ("latitude" %in% names(df_norm)) as.numeric(df_norm$latitude) else NA_real_
      lon <- if ("lon" %in% names(df_norm)) as.numeric(df_norm$lon) else if ("longitude" %in% names(df_norm)) as.numeric(df_norm$longitude) else NA_real_
      species <- if ("species" %in% names(df_norm)) df_norm$species else NA_character_
      common <- if ("commonname" %in% names(df_norm)) df_norm$commonname else NA_character_

      uid_vec <- if (any(is.finite(lat) & is.finite(lon))) {
        v <- rep(NA_character_, nrow(df_norm))
        ok <- is.finite(lat) & is.finite(lon)
        v[ok] <- mapply(.make_uid, species[ok], common[ok], lat[ok], lon[ok], USE.NAMES = FALSE)
        v
      } else {
        rep(NA_character_, nrow(df_norm))
      }
      df_norm$..uid <- uid_vec

      uid_map <- df_norm %>%
        dplyr::filter(!is.na(..uid)) %>%
        tidyr::unite("pathKey", dplyr::all_of(levs), sep = "/", remove = FALSE) %>%
        dplyr::group_by(pathKey) %>%
        dplyr::summarise(uids = list(unique(..uid)), .groups = "drop")

      # 4) Node -> list
      d3_data <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "name", childrenName = "children")

      # 5) Inject month / uids to leaves
      inject_meta <- function(node, trail = character(0)) {
        vals <- c(trail, node$name)
        if (!is.null(node$children) && length(node$children)) {
          node$children <- lapply(node$children, inject_meta, trail = vals)
        } else {
          vals_no_root <- if (length(vals) >= 2) vals[-1] else vals
          key <- paste(vals_no_root[seq_len(min(length(vals_no_root), length(levs)))], collapse = "/")
          # month
          idx_m <- match(key, month_map$pathKey)
          if (!is.na(idx_m)) node$month <- as.integer(month_map$month[idx_m])
          # uids
          idx_u <- match(key, uid_map$pathKey)
          if (!is.na(idx_u)) node$uids <- uid_map$uids[[idx_u]]
        }
        node
      }
      d3_data <- inject_meta(d3_data)

      # 6) Versioned JS
      js_src <- "www/force_tree_expandable.js"
      js_mtime <- tryCatch(file.info(js_src)$mtime, error = function(e) Sys.time())
      js_ver <- digest::digest(js_mtime, algo = "xxhash64")
      js_tmp <- file.path(tempdir(), sprintf("force_tree_expandable_%s.js", js_ver))
      if (!file.exists(js_tmp)) {
        dir.create(dirname(js_tmp), recursive = TRUE, showWarnings = FALSE)
        file.copy(js_src, js_tmp, overwrite = TRUE)
      }

      key <- tree_cache_key()
      render_key <- paste(key, "jsver=", js_ver, "init=kingdoms", "skipTaxa=1")

      r2d3::r2d3(
        data = d3_data,
        script = js_tmp,
        options = list(
          levels = levs,
          render_key = render_key,
          initial_view = "kingdoms",

          # <U+2193><U+2193><U+2193> Initial overall downward offset (pixels, JS needs to read initial_y_offset and apply to zoom initial transform)
          initial_y_offset = 24,
          link_dist = 18,
          node_r_root = 10, node_r_internal = 6, node_r_leaf_big = 8, node_r_collapsed = 7,
          zoom_min = 0.4, zoom_max = 3,
          click_input = ns("rtree_click"),
          leaf_click_input = NS(map_module_id)("rtree_click_from_js"),
          highlight_color = "#ffd700",
          highlight_width = 3
        )
      )
    }) |>
      bindCache(tree_cache_key()) |>
      bindEvent(tree_cache_key(), ignoreInit = FALSE)

    # Tree node click (path <U+2192> last level name) <U+2192> notify map module
    observeEvent(input$rtree_click, {
      val <- as.character(input$rtree_click)
      if (is.null(val) || !nzchar(val)) {
        return()
      }
      parts <- strsplit(val, "/", fixed = TRUE)[[1]]
      nm <- if (length(parts)) tail(parts, 1) else NA_character_
      if (is.na(nm) || !nzchar(nm)) {
        return()
      }
      session$sendCustomMessage(NS(map_module_id)("focus_from_tree"), list(name = nm))
    })
  })
}

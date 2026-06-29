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

left_sidebar_table1_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("tree_wrap"),
    style = "position:relative; width:100%; height:72vh; overflow:hidden; background:none;",
    r2d3::d3Output(ns("radial_tree"), height = "100%", width = "100%")
  )
}

left_sidebar_table1_server <- function(
    id, biodiv = NULL, levels = NULL, value_col = NULL, root = "Biodiversity",
    map_module_id = "bioMap" # <U+2605> Right map module id (must match actual)
    ) {
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

    # Cache key: includes level columns + sighting date + JS mtime
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

      # 2) Latest sighting month, inject into leaves
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

      d3_data <- data.tree::ToListExplicit(tree, unname = TRUE, nameName = "name", childrenName = "children")

      inject_month <- function(node, trail = character(0)) {
        vals <- c(trail, node$name)
        if (!is.null(node$children) && length(node$children)) {
          node$children <- lapply(node$children, inject_month, trail = vals)
        } else {
          vals_no_root <- if (length(vals) >= 2) vals[-1] else vals
          key <- paste(vals_no_root[seq_len(min(length(vals_no_root), length(levs)))], collapse = "/")
          idx <- match(key, month_map$pathKey)
          if (!is.na(idx)) node$month <- as.integer(month_map$month[idx])
        }
        node
      }
      d3_data <- inject_month(d3_data)

      # 3) Use "versioned" JS to force refresh
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
          link_dist = 18,
          node_r_root = 10, node_r_internal = 6, node_r_leaf_big = 8, node_r_collapsed = 7,
          zoom_min = 0.4, zoom_max = 3,
          # Input name passed back to R #
          click_input = ns("rtree_click"),
          # Leaf click: direct to right map module input
          leaf_click_input = NS(map_module_id)("rtree_click_from_js")
        )
      )
    }) |>
      bindCache(tree_cache_key()) |>
      bindEvent(tree_cache_key(), ignoreInit = FALSE)

    # Fallback: any node click (path) <U+2192> get last level name <U+2192> send to map handler via customMessage
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

      # Send to map module's handler, then handler writes to rtree_click_from_js
      session$sendCustomMessage(NS(map_module_id)("focus_from_tree"), list(name = nm))
    })
  })
}

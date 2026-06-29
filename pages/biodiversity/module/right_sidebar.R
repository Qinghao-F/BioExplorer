# pages/biodiversity/module/right_sidebar.R
# Right sidebar: collapsible + Search + Month legend + Kingdom legend (no emojis, bigger options)

library(shiny)

# ---- slimmer UI ----
right_sidebar_ui <- function(id) {
  ns <- NS(id)

  PANEL_W <- 200 # Adjust width here, e.g., 180 / 160

  div(
    id = ns("wrap"),
    class = "bio-filter-panel",
    style = sprintf(
      "width:%dpx; max-width:%dpx; padding:6px; background:#fff; box-sizing:border-box; margin:0;",
      PANEL_W, PANEL_W
    ),

    # Override outer styles that may widen panel; unify control widths
    tags$style(HTML(sprintf("
      .floating-panel{ min-width: unset !important; }
      #%s .form-control{ width:100%%; min-width:0; box-sizing:border-box; font-size:14px; }
      #%s .btn, #%s .btn-link{ padding-left:0; padding-right:0; }
      #%s hr{ margin:6px 0; }
    ", ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap")))),
    uiOutput(ns("header_ui")),
    uiOutput(ns("body_ui"))
  )
}

# data_all: reactive() -> data.frame (contains sightingdate/sighting_month/kingdom; lat/lon optional)
# data_map: reactive() -> data.frame (for map; contains lon/lat, preferably also sighting_month/kingdom)
right_sidebar_server <- function(id, data_all, data_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    show_body <- reactiveVal(TRUE)

    # ---- Search keywords (debounce 300ms) ----
    q <- debounce(
      reactive({
        x <- input$q
        if (is.null(x)) "" else tolower(trimws(x))
      }),
      300
    )

    # Expand/collapse
    observeEvent(input$toggle_filters,
      {
        show_body(!show_body())
      },
      ignoreInit = TRUE
    )

    # Header (no emoji)
    output$header_ui <- renderUI({
      is_open <- isTRUE(show_body())
      div(
        style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin-bottom:6px;",
        h4("Filters", style = "margin:0; font-weight:800; letter-spacing:.2px;"),
        actionLink(
          ns("toggle_filters"),
          label = if (is_open) "Hide" else "Show",
          icon  = icon(if (is_open) "chevron-up" else "chevron-down"),
          class = "btn btn-link",
          style = "margin-right:-6px;"
        )
      )
    })

    # ===== Month Legend UI (vertical layout; consistent with map opacity; larger font/size) =====
    month_control <- reactive({
      df_all <- data_all()
      mos <- integer(0)
      if (!is.null(df_all) && nrow(df_all)) {
        if ("sighting_month" %in% names(df_all)) {
          mos <- suppressWarnings(as.integer(df_all$sighting_month))
        } else if ("sightingdate" %in% names(df_all)) {
          s <- trimws(as.character(df_all$sightingdate))
          mos <- suppressWarnings(as.integer(substr(s, 6, 7)))
        }
        mos <- sort(unique(stats::na.omit(mos)))
      }
      mos <- setdiff(mos, 1L) # Remove this line if you want to show January

      # Month color scheme consistent with biomap.R (1..12)
      month_colors <- setNames(
        c(
          "#e41a1c", "#ff7f00", "#be4dea", "#55ca51", "#377eb8", "#984ea3",
          "#a65628", "#f781bf", "#1b9e77", "#7570b3", "#66a61e", "#e6ab02"
        ),
        as.character(1:12)
      )

      # #RRGGBB -> rgba(r,g,b,0.72)
      hex_to_rgba <- function(hex, alpha = 0.72) {
        rgb <- grDevices::col2rgb(hex)
        sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
      }

      values <- c("all", as.character(mos))
      sel <- if (!is.null(input$month) && input$month %in% values) input$month else "all"

      # Hidden radio buttons (for reset)
      hidden_rb <- radioButtons(
        inputId      = ns("month"),
        label        = NULL,
        choiceNames  = lapply(c("All months", month.name[mos]), tags$span),
        choiceValues = values,
        selected     = sel,
        inline       = TRUE
      )
      hidden_rb$attribs$style <- "display:none;"

      DOT <- 26 # Dot size increased from 22 to 26
      TXT <- 14 # Text font size increased from 12 to 14

      legend <- div(
        style = "display:flex; flex-direction:column; gap:6px; align-items:flex-start;",

        # All
        {
          is_sel <- identical(sel, "all")
          border <- if (is_sel) "3px solid #004d40" else "1.5px solid #c7c7c7"
          div(
            style = "display:flex; align-items:center; gap:8px; cursor:pointer;",
            div(
              style = sprintf(
                "width:%dpx;height:%dpx;border-radius:50%%;background:%s;border:%s;",
                DOT, DOT, hex_to_rgba("#c7c7c7", 0.72), border
              )
            ),
            span("All months", style = sprintf("font-size:%dpx;color:#355;", TXT)),
            onclick = sprintf("Shiny.setInputValue('%s','all',{priority:'event'})", ns("month"))
          )
        },

        # Each month
        lapply(mos, function(m) {
          m <- as.character(m)
          col <- month_colors[[m]]
          is_sel <- identical(sel, m)
          border <- if (is_sel) "3px solid #004d40" else "1.5px solid #c7c7c7"
          div(
            style = "display:flex; align-items:center; gap:8px; cursor:pointer;",
            div(
              style = sprintf(
                "width:%dpx;height:%dpx;border-radius:50%%;background:%s;border:%s;",
                DOT, DOT, hex_to_rgba(col, 0.72), border
              )
            ),
            span(month.name[as.integer(m)], style = sprintf("font-size:%dpx;color:#355;", TXT)),
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("month"), m)
          )
        })
      )

      tagList(hidden_rb, legend)
    })

    # ===== Kingdom Legend UI (vertical layout; All=circle with diamond inside; larger font/size) =====
    kingdom_control <- reactive({
      df_all <- data_all()
      ks <- character(0)
      if (!is.null(df_all) && nrow(df_all) && "kingdom" %in% names(df_all)) {
        ks <- trimws(as.character(df_all$kingdom))
        ks <- sort(unique(stats::na.omit(ks[nzchar(ks)])))
      }

      values <- c("all", ks)
      sel <- if (!is.null(input$kingdom) && input$kingdom %in% values) input$kingdom else "all"

      # Shape rule consistent with map
      shape_of <- function(k) {
        k_lower <- tolower(k)
        if (k_lower %in% c("animalia", "fungi")) "diamond" else "circle"
      }
      color_rgba <- "rgba(38,166,154,0.72)" # #26a69a @ 0.72

      # Hidden radio buttons (for reset)
      hidden_rb <- radioButtons(
        inputId      = ns("kingdom"),
        label        = NULL,
        choiceNames  = lapply(c("All kingdoms", ks), tags$span),
        choiceValues = values,
        selected     = sel,
        inline       = TRUE
      )
      hidden_rb$attribs$style <- "display:none;"

      DOT <- 26 # Outer circle size
      DIAM <- 20 # Inner diamond side length (one size larger than 16)
      TXT <- 14 # Text size increased

      draw_shape <- function(shape, active, title) {
        border <- if (active) "#004d40" else "#9aa2a9"
        if (shape == "circle") {
          HTML(sprintf(
            "<div title='%s' style='width:%dpx;height:%dpx;border-radius:50%%;background:%s;border:2px solid %s;'></div>",
            htmltools::htmlEscape(title), DOT, DOT, color_rgba, border
          ))
        } else { # diamond
          HTML(sprintf(
            "<div title='%s' style='width:%dpx;height:%dpx;position:relative;'>
               <div style='position:absolute;left:50%%;top:50%%;width:%dpx;height:%dpx;transform:translate(-50%%,-50%%) rotate(45deg);background:%s;border:2px solid %s;'></div>
             </div>",
            htmltools::htmlEscape(title), DOT, DOT, DIAM, DIAM, color_rgba, border
          ))
        }
      }

      # All: circle with diamond inside
      draw_all_icon <- function(active) {
        border <- if (active) "3px solid #004d40" else "1.5px solid #c7c7c7"
        HTML(sprintf(
          "<div title='All kingdoms' style='position:relative;width:%dpx;height:%dpx;border-radius:50%%;background:rgba(199,199,199,0.72);border:%s;'>
             <div style='position:absolute;left:50%%;top:50%%;width:%dpx;height:%dpx;transform:translate(-50%%,-50%%) rotate(45deg);background:%s;border:2px solid #9aa2a9;'></div>
           </div>",
          DOT, DOT, border, DIAM - 6, DIAM - 6, color_rgba
        ))
      }

      legend <- div(
        style = "display:flex; flex-direction:column; gap:6px; align-items:flex-start;",

        # All (circle with diamond inside)
        div(
          style = "display:flex; align-items:center; gap:8px; cursor:pointer;",
          draw_all_icon(active = identical(sel, "all")),
          span("All kingdoms", style = sprintf("font-size:%dpx;color:#355;", TXT)),
          onclick = sprintf("Shiny.setInputValue('%s','all',{priority:'event'})", ns("kingdom"))
        ),

        # Each kingdom
        lapply(ks, function(k) {
          shape <- shape_of(k)
          div(
            style = "display:flex; align-items:center; gap:8px; cursor:pointer;",
            draw_shape(shape, active = identical(sel, k), title = k),
            span(k, style = sprintf("font-size:%dpx;color:#355;", TXT)),
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("kingdom"), k)
          )
        })
      )

      tagList(hidden_rb, legend)
    })

    # ===== Main body: Search + Month + Kingdom + Summary/Reset =====
    output$body_ui <- renderUI({
      if (!isTRUE(show_body())) {
        return(NULL)
      }

      tagList(
        div(
          style = "font-size:13px; color:#607d8b; margin-top:-2px; margin-bottom:8px;",
          "Refine biodiversity sightings"
        ),
        tags$hr(style = "margin:8px 0; border-color:#eee;"),

        # Search (no emoji)
        div(
          style = "margin-bottom:8px; display:flex; flex-direction:column; gap:6px;",
          div(style = "display:flex; align-items:center; gap:6px;", strong("Search")),
          textInput(
            inputId = ns("q"),
            label = NULL,
            placeholder = "Type keywords (e.g., eucalyptus, bird)",
            width = "100%"
          ),
          actionLink(ns("clear_q"),
            label = "Clear search",
            class = "btn btn-link",
            style = "padding-left:0; margin-top:-6px;"
          )
        ),
        tags$hr(style = "margin:8px 0; border-color:#eee;"),

        # Month (no emoji)
        div(style = "margin-bottom:6px; display:flex; align-items:center; gap:6px;", strong("Month")),
        div(class = "bio-month", month_control()),
        tags$hr(style = "margin:8px 0; border-color:#eee;"),

        # Kingdom (no emoji)
        div(style = "margin-bottom:6px; display:flex; align-items:center; gap:6px;", strong("Kingdom")),
        div(class = "bio-kingdom", kingdom_control()),
        tags$hr(style = "margin:10px 0; border-color:#eee;"),

        # Footer: summary on top, reset at bottom (vertical layout)
        div(
          style = "display:flex; flex-direction:column; align-items:flex-start; gap:6px;",
          uiOutput(ns("summary")),
          actionLink(ns("reset"),
            label = "Reset filters",
            icon  = icon("rotate-left"),
            class = "btn btn-link",
            style = "padding-left:0; margin-top:2px;"
          )
        )
      )
    })

    # Clear search
    observeEvent(input$clear_q,
      {
        updateTextInput(session, "q", value = "")
      },
      ignoreInit = TRUE
    )

    # Reset
    observeEvent(input$reset,
      {
        updateRadioButtons(session, "month", selected = "all")
        updateRadioButtons(session, "kingdom", selected = "all")
        updateTextInput(session, "q", value = "")
      },
      ignoreInit = TRUE
    )

    # ===== Filtering: Month / Kingdom / Keyword =====
    filtered <- reactive({
      dff <- data_map()
      if (is.null(dff) || !nrow(dff)) {
        return(dff)
      }

      # Month
      val_m <- input$month
      if (!is.null(val_m) && !identical(val_m, "all") && "sighting_month" %in% names(dff)) {
        sel_m <- suppressWarnings(as.integer(val_m))
        if (!is.na(sel_m)) dff <- dff[dff$sighting_month == sel_m, , drop = FALSE]
      }

      # kingdom
      if ("kingdom" %in% names(dff)) {
        val_k <- input$kingdom
        if (!is.null(val_k) && !identical(val_k, "all")) {
          k_col <- trimws(as.character(dff$kingdom))
          dff <- dff[k_col == val_k, , drop = FALSE]
        }
      }

      # Keywords (each term must match at least one field)
      kw <- q()
      if (nzchar(kw) && nrow(dff)) {
        terms <- strsplit(kw, "\\s+")[[1]]
        cols <- intersect(
          c(
            "display_name", "commonname", "species", "taxa", "genus", "family",
            "order", "class", "phylum", "kingdom", "identificationnotes",
            "dataresourcename", "location"
          ),
          names(dff)
        )
        if (length(cols)) {
          nz <- function(x) ifelse(is.na(x), "", x)
          keep <- rep(TRUE, nrow(dff))
          low <- lapply(cols, function(cl) tolower(nz(dff[[cl]])))
          for (t in terms) {
            hit_t <- Reduce(`|`, lapply(low, function(v) grepl(t, v, fixed = TRUE)))
            hit_t[is.na(hit_t)] <- FALSE
            keep <- keep & hit_t
          }
          dff <- dff[keep, , drop = FALSE]
        }
      }

      # When month/kingdom are both 'all' and no search, indicate map should show "ALL"
      attr(dff, "bio_show_all") <- (is.null(input$month) || identical(input$month, "all")) &&
        (is.null(input$kingdom) || identical(input$kingdom, "all")) &&
        !nzchar(kw)
      dff
    })

    # Summary
    output$summary <- renderUI({
      dff <- filtered()
      n <- if (!is.null(dff)) nrow(dff) else 0
      div(
        style = "font-size:13px; color:#607d8b;",
        sprintf("Showing %s record%s", format(n, big.mark = ","), ifelse(n == 1, "", "s"))
      )
    })

    # Return for map to use
    return(filtered)
  })
}

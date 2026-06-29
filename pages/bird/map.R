bird_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # External CSS and JS
    includeCSS("assets/bio.css"),
    includeScript("assets/bio.js"),
    div(
      id = "__bird_root__",
      class = "bio-layout",
      style = "display:flex; flex-direction:row; width:100%; height:calc(100vh - 44px); margin:0; padding:0; overflow:hidden; border:none; position:relative;",

      # --- Left Control Panel: Top/Bottom Layout (60%) ---
      div(
        class = "bio-side-panel",
        style = "
          width:60%; height:100%; padding:16px;
          display:flex; flex-direction:column;
          gap:16px; overflow:hidden;
          background-color: #e0f2f1;
        ",
        div(
          style = "flex:1; min-height:0; display:flex; flex-direction:row; gap:16px; overflow:hidden;",

          # Left: heatmap
          div(
            style = "flex:3; min-width:0; background-color: white; padding:10px;
             border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            h4(
              style = "color:#00695c; margin-top:0; margin-bottom:8px;",
              "Species<U+2013>Park Sightings Heatmap"
            ),
            plotOutput(ns("species_heatmap"), height = "calc(100% - 40px)")
          ),

          # Right: time series line chart
          div(
            style = "flex:1; min-width:0; background-color: white; padding:10px;
             border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            uiOutput(ns("time_series_title")),
            plotOutput(ns("time_series_plot"), height = "calc(100% - 40px)")
          )
        ),
        div(
          style = "flex:1; min-height:0; display:flex; flex-direction:row; gap:16px; overflow:hidden;",

          # 1. Tableau Embed
          # 1. Tableau Embed using iframe
          div(
            style = "flex:2; min-width:0; background-color:white; padding:10px; border-radius:8px; box-shadow:0 1px 3px rgba(0,0,0,0.05);",
            h4(
              style = "color:#00695c; margin-top:0; margin-bottom:8px;",
              "Bird Sightings Overview"
            ),
            tags$iframe(
              src = "https://public.tableau.com/views/Book13_17615627758840/Sheet1?:showVizHome=no&:embed=true",
              width = "100%",
              height = "600px",
              frameborder = "0",
              style = "border:none; width:100%; height:100%;"
            )
          ),
          # 2. Bar Chart (Top Species)
          div(
            style = "flex:2; min-width:0; background-color: white; padding:10px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
            h4(style = "color:#00695c; margin-top:0;", uiOutput(ns("top_species_title"))),
            plotOutput(ns("top_species_plot"), height = "calc(100% - 30px)")
          )
        )
      ),

      # --- Right Map (40%) ---
      div(
        class = "bio-map-container",
        style = "
    width:40%; height:100%;
    margin:0; padding:0; border:none; box-sizing:border-box;
    display:flex; flex-direction:column;
    background-color: #e9ecef;
    position:relative;
  ",
        div(
          style = "background-color: #00695c; color:white; padding: 10px; text-align: center; font-weight: bold;",
          "Bird Sightings Distribution Map"
        ),
        leafletOutput(ns("map"), height = "calc(100% - 40px)"),
        tags$img(
          src = "compass.png",
          style = "
      position:absolute;
      top:55px;
      right:10px;
      width:60px;
      height:auto;
      opacity:0.85;
      border-radius:50%;
      background:rgba(255,255,255,0.7);
      padding:5px;
      z-index:9999;
    "
        )
      )
    )
  )
}

bird_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Data Preparation (Unchanged) ----
    # ... (data preparation code remains unchanged) ...
    if (!exists("points_data")) {
      stop("Global points_data not found! Please load it in global.R")
    }

    # Check if Sighting Date exists and convert to Date
    if ("Sighting Date" %in% colnames(points_data)) {
      points_data <- points_data %>%
        mutate(`Sighting Date` = as.Date(`Sighting Date`))
    } else {
      warning("<U+26A0><U+FE0F> 'Sighting Date' column not found in points_data <U+2014> time series chart will be empty.")
    }

    # Aggregate to (lat, lon, site_name)
    agg_data <- points_data %>%
      group_by(lat, lon, site_name) %>%
      summarise(
        total_count = sum(value, na.rm = TRUE),
        species = paste(unique(name), collapse = ", "),
        .groups = "drop"
      )

    bird_colors <- c("#d73027", "#f46d43", "#fdae61", "#a8d5ba", "#66bfa7", "#2a9d8f")

    agg_data <- agg_data %>%
      arrange(desc(total_count)) %>%
      mutate(color = bird_colors[(seq_len(nrow(.)) - 1) %% length(bird_colors) + 1])

    # ---- Map (Unchanged) ----
    output$map <- renderLeaflet({
      leaflet(agg_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          ~lon, ~lat,
          layerId = ~site_name,
          popup = ~ paste0(
            "<b>Park Name:</b> ", site_name, "<br>",
            "<b>Total Sightings:</b> ", total_count
          ),
          radius = ~ ifelse(is.na(total_count) | total_count == 0, 5, 2 + (total_count / 80)),
          color = ~color,
          fillOpacity = 0.7
        ) %>%
        addScaleBar(position = "bottomleft")
    })

    # Default selected park
    default_site <- points_data %>%
      group_by(site_name) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      slice(1) %>%
      pull(site_name)

    selected_site <- reactiveVal(default_site)

    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click$id)) selected_site(click$id)
    })

    # ---- Dynamic Title Rendering (Translated) ----
    output$top_species_title <- renderUI({
      h4(style = "color:#00695c; margin-top:0;", paste("Top 10 Species in", selected_site()))
    })

    output$time_series_title <- renderUI({
      h6(style = "color:#00695c; margin-top:0;", paste("Sightings Trend for", selected_site()))
    })

    # ---- Bar Chart (Translated Axis Labels) ----
    output$top_species_plot <- renderPlot({
      site_data <- points_data %>%
        filter(site_name == selected_site()) %>%
        group_by(name) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = 10)

      ggplot(site_data, aes(x = reorder(name, total), y = total)) +
        geom_col(fill = "#2a9d8f") +
        coord_flip() +
        labs(x = "Species", y = "Sightings Count") + # Translated
        theme_minimal(base_size = 13) +
        theme(
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    })

    # ---- Heatmap (Translated Axis Labels) ----
    output$species_heatmap <- renderPlot({
      # Aggregation
      heat_data <- points_data %>%
        group_by(name, site_name) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

      # Select top 20 species
      top_species <- heat_data %>%
        group_by(name) %>%
        summarise(total_species = sum(total, na.rm = TRUE)) %>%
        arrange(desc(total_species)) %>%
        slice_head(n = 20) %>%
        pull(name)

      heat_data <- heat_data %>%
        filter(name %in% top_species)

      # Multi-color heatmap
      ggplot(heat_data, aes(x = name, y = site_name, fill = total)) +
        geom_tile(color = "white") +
        scale_fill_gradientn(
          colours = c(
            "#a8d5ba", # Light Green
            "#fdae61", # Orange
            "#f46d43" # Dark Orange
          ),
          name = "Sightings Count" # Translated
        ) +
        labs(x = "Species", y = "Park") + # Translated
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    })

    # ---- Time Series Plot (Translated Axis Labels) ----
    output$time_series_plot <- renderPlot({
      req("Sighting Date" %in% colnames(points_data))

      time_data <- points_data %>%
        filter(site_name == selected_site()) %>%
        group_by(`Sighting Date`) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(`Sighting Date`)

      ggplot(time_data, aes(x = `Sighting Date`, y = total)) +
        geom_line(color = "#00695c", linewidth = 1.2) +
        geom_point(color = "#004d40", size = 2) +
        labs(x = "Date", y = "Total Sightings") +
        scale_x_date(
          date_labels = "%b %d",
          date_breaks = "1 week"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_line(size = 0.5)
        )
    })
  })
}

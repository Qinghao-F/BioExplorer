# pages/biodiversity/map.R
# Biodiversity map using global.R helper

biodiversity_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 加 wrapper，并移除一切边框、内外边距
    div(
      style = "border:none; box-shadow:none; padding:0; margin:0;",
      leafletOutput(ns("bio_map"), height = "100vh", width = "100%")
    ),
    # 全局确保 Leaflet 本身无边框
    tags$style(HTML("
      .leaflet, .leaflet-container {
        border: none !important;
        box-shadow: none !important;
      }
    "))
  )
}


biodiversity_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 调用 global.R 中的工具函数来读取和标准化数据
    df <- read_and_normalize_points("dataset/biodiversity.csv")

    output$bio_map <- renderLeaflet({
      if (!nrow(df)) {
        # 如果没有数据，显示墨尔本默认视图
        leaflet() |>
          addProviderTiles(providers$CartoDB.DarkMatter) |>
          setView(lng = 144.9631, lat = -37.8136, zoom = 10)
      } else {
        leaflet(df) |>
          addProviderTiles(providers$CartoDB.DarkMatter) |>
          setView(
            lng = mean(df$lon, na.rm = TRUE),
            lat = mean(df$lat, na.rm = TRUE),
            zoom = 12
          ) |>
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            label = ~name,
            popup = ~ paste0("<b>", name, "</b><br/>Category: ", category),
            radius = 6, fillOpacity = 0.8,
            clusterOptions = markerClusterOptions()
          )
      }
    })
  })
}
